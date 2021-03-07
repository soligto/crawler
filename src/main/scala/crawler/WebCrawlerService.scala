package crawler

import cats.effect.{ BracketThrow, Concurrent }
import cats.{ ApplicativeError, Monad }
import fs2.{ Pipe, Stream }
import org.http4s.client.Client
import org.http4s.{ Request, Response, Uri }

trait WebCrawlerService[F[_]] {
  def getTitles(request: TitlesRequest): F[TitlesResponse]
}

case class GetTitleAttempt[F[_]](
  uri: Either[String, Uri],
  response: Option[Response[F]],
  result: Either[CrawlerError, Tag]
)

object WebCrawlerService {
  def apply[F[_]: Monad: Concurrent: BracketThrow](
    client: Client[F],
    titleParser: Parser[Tag],
    parserPipe: Parser[Tag] => Pipe[F, Array[Byte], Either[CrawlerError, Tag]]
  )(implicit ApplicativeError: ApplicativeError[F, Throwable]): WebCrawlerService[F] =
    new WebCrawlerService[F] {

      /**
       * Конвертит response body в стрим и парсит его.
       * Стрим берёт первое распарсенное значение, а в случае отсутствия элемента возвращает ошибку Title not found.
       */
      private def responseToTitle(uri: Uri, response: Response[F]): Stream[F, GetTitleAttempt[F]] = {
        response.body.chunks
          .map(_.toArray)
          .through(parserPipe(titleParser))
          .take(1)
          .map { tag =>
            GetTitleAttempt[F](Right(uri), Some(response), tag)
          }
          .lastOr(GetTitleAttempt[F](Right(uri), Some(response), Left(NotFoundError("Title not found"))))
      }

      /**
       * Добавляет попытку получения заголовка GetTitleAttempt в TitlesResponse
       * В зависимости от наличия ошибок парсинга, элемент попадает либо в массив titles, либо в массив errors.
       */
      private def attemptToTitlesResponse(titles: TitlesResponse, attempt: GetTitleAttempt[F]): TitlesResponse = {
        attempt match {
          case GetTitleAttempt(uri, response, Left(error)) =>
            error match {
              case error: UnexpectedError =>
                titles.copy(errors =
                  TitleError(uri, "Unexpected error", Some(error)) +: titles.errors
                )
              case error: ParserError     =>
                titles.copy(errors =
                  TitleError(uri, "Error during parsing html", Some(error)) +: titles.errors
                )
              case error: BadRequestError =>
                titles.copy(errors =
                  TitleError(uri, "Request part is malformed", Some(error)) +: titles.errors
                )
              case error: NotFoundError   =>
                titles.copy(errors =
                  TitleError(
                    uri,
                    s"Response code ${response.map(_.status.toString()).getOrElse("")}",
                    Some(error)
                  ) +: titles.errors
                )
            }
          case GetTitleAttempt(uri @ Left(_), _, _)        =>
            titles.copy(errors = TitleError(uri, s"Not a uri", None) +: titles.errors)
          case GetTitleAttempt(Right(uri), _, Right(tag))  =>
            titles.copy(titles = Title(uri, tag.content) +: titles.titles)
        }
      }

      /**
       * Клиент отправляет запросы по каждому uri. Каждый response body преобразуется в стрим.
       * Стрим парсится pipe'ом, который возвращает элементы по мере их появления.
       * Как только заголовок найден, дальнейшая загрузка и парсинг response body не осуществляется.
       * Если во время подготовки запроса или обращения на uri возникает ошибка, то она сохраняется в GetTitleAttempt.
       * Вычисление результатов осуществляется конкурентно с помощью parJoinUnbounded.
       */
      override def getTitles(request: TitlesRequest): F[TitlesResponse] = {
        if (request.uris.isEmpty) {
          ApplicativeError.raiseError(BadRequestError("Uri list is empty"))
        } else {
          Stream.fromIterator {
            request.uris.map { uri =>
              {
                for {
                  uri      <- Stream.eval(Uri.fromString(uri) match {
                                case Left(error) => ApplicativeError.raiseError[Uri](BadRequestError(error.getMessage()))
                                case Right(uri)  => Monad[F].pure(uri)
                              })
                  response <- client.stream(Request(uri = uri))
                  value    <- responseToTitle(uri, response)
                } yield value
              }.attempt.map {
                case Left(error)    => GetTitleAttempt[F](Left(uri), None, Left(UnexpectedError(error.getMessage)))
                case Right(attempt) => attempt
              }
            }.iterator
          }.parJoinUnbounded
            .fold(TitlesResponse(Vector.empty, Vector.empty))(attemptToTitlesResponse)
            .compile
            .lastOrError
        }
      }
    }
}
