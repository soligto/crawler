package crawler

import cats.effect.{ContextShift, IO}
import fs2.Stream
import org.http4s.Response
import org.http4s.client.Client
import org.http4s.implicits.http4sLiteralsSyntax
import org.scalamock.scalatest.MockFactory

class WebCrawlerServiceTest extends Test with MockFactory {
  def getService(client: Client[IO])(implicit cs: ContextShift[IO]): WebCrawlerService[IO] = {
    WebCrawlerService[IO](client, TagParser("title"), TagParserPipe.apply[IO])
  }

  "WebCrawlerService" should {
    "return a title of the requested web page" in {
      (client: Client[IO], context: Context) => {
        import context._
        (client.stream _).expects(*).returning(Stream(Response[IO](
          body = Stream("<html>", "<title>", "test title", "</title>").flatMap { part =>
            Stream.fromIterator[IO](part.getBytes.iterator)
          }
        )))

        val service = getService(client)
        for {
          titlesResponse <- service.getTitles(TitlesRequest(Vector("http://url")))
        } yield {
          titlesResponse.errors shouldBe Vector()
          titlesResponse.titles shouldBe Vector(Title(uri"http://url", "test title"))
        }
      }
    }

    "return an error because of the empty response" in {
      (context: Context, client: Client[IO]) => {
        import context._
        (client.stream _).expects(*).returning(Stream(Response[IO](
          body = Stream("<html>", "</html>").flatMap { part =>
            Stream.fromIterator[IO](part.getBytes.iterator)
          }
        )))

        val service = getService(client)
        for {
          titlesResponse <- service.getTitles(TitlesRequest(Vector("http://url")))
        } yield {
          titlesResponse.errors shouldBe Vector(TitleError(Right(uri"http://url"), "Title not found", "Response code 200 OK"))
          titlesResponse.titles shouldBe Vector()
        }
      }
    }

    "return an error because of the error response" in {
      (context: Context, client: Client[IO]) => {
        import context._
        (client.stream _).expects(*).returning(Stream.eval(IO.raiseError(new RuntimeException("connect error"))))

        val service = getService(client)
        for {
          titlesResponse <- service.getTitles(TitlesRequest(Vector("http://url")))
        } yield {
          titlesResponse.errors shouldBe Vector(TitleError(Left("http://url"), "Unexpected error", "connect error"))
          titlesResponse.titles shouldBe Vector()
        }
      }
    }
  }
}
