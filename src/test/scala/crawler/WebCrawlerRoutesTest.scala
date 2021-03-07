package crawler

import cats.effect.IO
import io.circe.parser._
import io.circe.syntax.EncoderOps
import org.http4s.circe.jsonEncoder
import org.http4s.client.dsl.io.http4sWithBodySyntax
import org.http4s.dsl.io.POST
import org.http4s.implicits.{ http4sLiteralsSyntax, _ }

class WebCrawlerRoutesTest extends Test {
  "WebCrawlerRoutes" should {
    "return a title of the requested web page" in { () =>
      {
        val serviceResult                  = TitlesResponse(Vector(Title(uri"http://url", "title")), Vector())
        val service: WebCrawlerService[IO] = _ => IO.pure(serviceResult)

        for {
          request        <- POST(TitlesRequest(Vector("http://url")).asJson, uri"/titles")
          response       <- new WebCrawlerRoutes[IO].routes(service).orNotFound.run(request)
          body           <- response.body.compile.toVector.map(bytes => new String(bytes.toArray))
          titlesResponse <- IO(decode[TitlesResponse](body)).flatMap {
                              case Left(error)   => IO.raiseError(error)
                              case Right(result) => IO.pure(result)
                            }
        } yield {
          titlesResponse shouldBe serviceResult
        }
      }
    }
  }
}
