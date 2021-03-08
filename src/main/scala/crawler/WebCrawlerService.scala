package crawler

import cats.effect.Concurrent
import cats.syntax.functor._
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
object GetTitleAttempt {
  def apply[F[_]](uri: String, error: CrawlerError): GetTitleAttempt[F] = {
    GetTitleAttempt[F](Left(uri), None, Left(error))
  }
}

object WebCrawlerService {
  type GetTitleAttempt = Either[TitleError, Title]

  def apply[F[_]: Monad: Concurrent](
    client: Request[F] => Stream[F, Response[F]],
    titleParser: F[Parser[Tag]],
    parserPipe: Parser[Tag] => Pipe[F, Array[Byte], Either[CrawlerError, Tag]]
  )(implicit ApplicativeError: ApplicativeError[F, Throwable]): WebCrawlerService[F] =
    new WebCrawlerService[F] {

      /**
       * Конвертит response body в стрим и парсит его.
       * Стрим берёт первое распарсенное значение, а в случае отсутствия элемента возвращает ошибку Title not found.
       */
      private def responseToTitle(uri: Uri, response: Response[F]): Stream[F, GetTitleAttempt] = {
        Stream.eval(titleParser).flatMap { parser =>
          response.body.chunks
            .map(_.toArray)
            .through(parserPipe(parser))
            .take(1)
            .map {
              case Left(error)  => Left(TitleError(uri.renderString, error))
              case Right(title) => Right(Title(uri, title.content))
            }
            .lastOr(Left(TitleError(uri.renderString, NotFoundError("Title not found"))))
        }
      }

      /**
       * Осуществляет запрос на заданный uri
       */
      private def getTitle(uri: String): Stream[F, GetTitleAttempt] = {
        {
          for {
            uri      <- Stream.eval(Uri.fromString(uri) match {
                          case Left(error) => ApplicativeError.raiseError[Uri](BadRequestError(error.getMessage))
                          case Right(uri)  => Monad[F].pure(uri)
                        })
            response <- client(Request(uri = uri))
            value    <- responseToTitle(uri, response)
          } yield value
        }.attempt.map {
          case Left(error)    => Left(TitleError(uri, UnexpectedError(error.getMessage)))
          case Right(attempt) => attempt
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
          Stream.emits {
            request.uris.map(getTitle)
          }.parJoinUnbounded
            .groupAdjacentBy(_.isRight)
            .compile
            .to(Map)
            .map { results =>
              val titles = results.get(true).fold(Vector.empty[Title])(_.collect { case Right(title) => title }.toVector)
              val errors = results.get(false).fold(Vector.empty[TitleError])(_.collect { case Left(error) => error }.toVector)
              TitlesResponse(titles, errors)
            }
        }
      }
    }
}
