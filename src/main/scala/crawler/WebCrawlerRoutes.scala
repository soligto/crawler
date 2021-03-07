package crawler

import cats.effect.Async
import cats.syntax.all._
import io.circe.syntax.EncoderOps
import org.http4s.HttpRoutes
import org.http4s.circe.CirceEntityCodec._
import org.http4s.dsl.Http4sDsl

class WebCrawlerRoutes[F[_]: Async] extends Http4sDsl[F] {
  def routes(service: WebCrawlerService[F]): HttpRoutes[F] =
    HttpRoutes.of[F] {
      case req @ POST -> Root / "titles" =>
        req
          .decode[TitlesRequest] { request =>
            service.getTitles(request).flatMap {
              case titles if titles.errors.isEmpty => Ok(titles.asJson)
              case titles                          => BadRequest(titles.asJson)
            }
          }
          .handleErrorWith {
            case e: BadRequestError =>
              val error: CrawlerError = e
              BadRequest(error.asJson)
            case e: Exception =>
              val error: CrawlerError = UnexpectedError(e.getMessage)
              InternalServerError(error.asJson)
          }
    }
}
