package crawler

import cats.effect._
import cats.syntax.all._
import io.circe.syntax.EncoderOps
import org.http4s._
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
              case titles => InternalServerError(titles.asJson)
            }
          }
          .handleErrorWith {
            case e: BadRequestError =>
              BadRequest(e.asJson)
            case e: Exception       =>
              BadRequest(BadRequestError(e.getMessage + e.getStackTrace.toList.mkString("\n")).asJson)
          }
    }
}
