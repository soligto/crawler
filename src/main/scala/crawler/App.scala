package crawler

import cats.effect.{ ExitCode, IO, IOApp }
import crawler.xml.{ AsyncXmlStreamParser, AsyncXmlTagEventHandler }
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder

object App extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    BlazeClientBuilder[IO](executionContext).resource.use { client =>
      BlazeServerBuilder[IO]
        .bindHttp(8080, "0.0.0.0")
        .withHttpApp {
          Router[IO](
            "/" -> {
              val titleParser: IO[Parser[Tag]] = IO.pure(RegexTagParser("title"))
              val parserPipe                   = ParserPipe.apply[IO, Tag] _
              new WebCrawlerRoutes[IO].routes(WebCrawlerService(client, titleParser, parserPipe))
            }
          ).orNotFound
        }
        .serve
        .compile
        .drain
        .as(ExitCode.Success)
    }
  }
}
