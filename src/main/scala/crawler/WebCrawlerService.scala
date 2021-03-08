package crawler

import cats.effect.Concurrent
import cats.syntax.functor._
import cats.{ ApplicativeError, Monad }
import fs2.{ Pipe, Stream }
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
       * Преобразует response body в Stream и парсит элементы Tag из его содержимого.
       * Найденный Tag преобразуются в GetTitleAttempt. Если Stream оказался пуст, то в результирующий Stream
       * помещается NotFoundError.
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
       * Осуществляет запрос на заданный uri для получения содержимого тега title
       */
      private def getTitle(uri: String): Stream[F, GetTitleAttempt] = {
        Uri.fromString(uri) match {
          case Left(error)      => Stream(Left(TitleError(uri, BadRequestError(error.getMessage))))
          case Right(parsedUri) => {
              for {
                response <- client(Request(uri = parsedUri))
                value    <- responseToTitle(parsedUri, response)
              } yield value
            }.handleErrorWith { error =>
              Stream(Left(TitleError(uri, UnexpectedError(error.getMessage))))
            }
        }
      }

      /**
       * Метод преобразует список uri в список Stream'ов с результатами получения содержимого title.
       * Каждый стрим вычисляется конкуренто с помощью parJoinUnbounded и далее группируется в два раздельных Chunk
       * с успешными результатами и ошибками.
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
