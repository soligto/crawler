package crawler

import cats.effect.{ Concurrent, IO }
import cats.syntax.functor._
import cats.syntax.flatMap._
import cats.{ ApplicativeError, Monad }
import fs2.{ Pipe, Stream }
import logstage.LogIO
import org.http4s.{ ParseFailure, Request, Response, Uri }

trait WebCrawlerService[F[_]] {
  def getTitles(request: TitlesRequest): F[TitlesResponse]
}

object WebCrawlerService {
  type GetTitleAttempt = Either[TitleError, Title]

  /**
   * Реализация  WebCrawlerService по умолчанию, использующая fs2.Stream для конкурентного вычисления содержимого
   * определенного тега по списку запрошенных страниц.
   * @param client           клиент, преобразующий http запрос в Stream с содержимым ответа
   * @param titleParser      парсер массива байт в список Tag.
   * @param parserPipe       pipe, преобразующая Stream массивов байт в Stream элементов типа Tag
   */
  def apply[F[_]: Monad: Concurrent: LogIO](
    client: Request[F] => Stream[F, Response[F]],
    titleParser: F[Parser[Tag]],
    parserPipe: Parser[Tag] => Pipe[F, Array[Byte], Either[CrawlerError, Tag]]
  )(implicit ApplicativeError: ApplicativeError[F, Throwable]): WebCrawlerService[F] =
    new WebCrawlerService[F] {

      /**
       * Осуществляет запрос на заданный uri и преобразует response body в Stream
       * элементов Tag, либо ошибок CrawlerError
       */
      private def getTag(uri: Uri, parserF: F[Parser[Tag]]): Stream[F, Either[CrawlerError, Tag]] = {
        {
          for {
            parser   <- Stream.eval(parserF)
            response <- client(Request(uri = uri))
            value    <- response.body.chunks
                          .map(_.toArray)
                          .through(parserPipe(parser))
          } yield value
        }.handleErrorWith { error =>
          Stream
            .emit(Left(UnexpectedError(error.getMessage)))
            .evalTap(_ => LogIO[F].error(s"Failed to parse tags from $uri: $error"))
        }
      }

      /**
       * Метод, трансформирующий список запрошенных uri в Stream.
       * Если строка не является Uri, то соответствующий элемент преобразуется в Stream от ошибки A.
       * Если строка является Uri, то она преобразуется функцией в Stream элементов B.
       */
      private def transformUriSeq[A, B](
        uris: Seq[String]
      )(f: Uri => Stream[F, Either[A, B]])(g: (String, ParseFailure) => A): Stream[F, Stream[F, Either[A, B]]] = {
        Stream.emits(uris.map { uri =>
          Uri.fromString(uri) match {
            case Right(uri)  => f(uri)
            case Left(error) =>
              Stream
                .emit(Left(g(uri, error)))
                .evalTap(_ => LogIO[F].error(s"Failed to parse $uri"))
          }
        })
      }

      /**
       * Метод преобразует список uri в Stream Title или TitleError, если title не удалось получить.
       */
      override def getTitles(request: TitlesRequest): F[TitlesResponse] = {
        if (request.uris.isEmpty) {
          ApplicativeError.raiseError(BadRequestError("Uri list is empty"))
        } else {
          transformUriSeq(request.uris) { uri =>
            getTag(uri, titleParser)
              .take(1)
              .map {
                case Left(error) => Left(TitleError(uri.renderString, error))
                case Right(tag)  => Right(Title(uri, tag.content))
              }
              .lastOr(Left(TitleError(uri.renderString, NotFoundError("Title not found"))))
              .evalTap {
                case Left(error)  => LogIO[F].error(s"Failed to find a title: $error")
                case Right(title) => LogIO[F].debug(s"Found a title: $title")
              }
          } { (uri, parseFailure) =>
            TitleError(uri, BadRequestError(parseFailure.getMessage()))
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
