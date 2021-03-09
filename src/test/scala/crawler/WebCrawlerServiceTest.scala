package crawler

import cats.effect.IO
import fs2.Stream
import org.http4s.implicits.http4sLiteralsSyntax
import org.http4s.{ Request, Response }

class WebCrawlerServiceTest extends Test {
  "WebCrawlerService" should {
    "return a title of the requested web page" in { (service: Test.ServiceProvider[IO]) =>
      {
        for {
          client <- IO { (_: Request[IO]) =>
                      Stream(Response[IO](body = Stream("<html>", "<title>", "test title", "</title>").flatMap { part =>
                        Stream.emits(part.getBytes)
                      }))
                    }
          titles <- service(client).getTitles(TitlesRequest(Vector("http://url")))
        } yield {
          titles.errors shouldBe Vector()
          titles.titles shouldBe Vector(Title(uri"http://url", "test title"))
        }
      }
    }

    "return an error because of the empty response" in { (service: Test.ServiceProvider[IO]) =>
      {
        for {
          client <- IO { (_: Request[IO]) =>
                      Stream(Response[IO](body = Stream("<html>", "</html>").flatMap { part =>
                        Stream.emits(part.getBytes)
                      }))
                    }
          titles <- service(client).getTitles(TitlesRequest(Vector("http://url")))
        } yield {
          titles.errors shouldBe Vector(TitleError("http://url", NotFoundError("Title not found")))
          titles.titles shouldBe Vector()
        }
      }
    }

    "return an error because of the error response" in { (service: Test.ServiceProvider[IO]) =>
      {
        for {
          client         <- IO((_: Request[IO]) => Stream.raiseError[IO](new RuntimeException("connect error")))
          titlesResponse <- service(client).getTitles(TitlesRequest(Vector("http://url")))
        } yield {
          titlesResponse.errors shouldBe Vector(TitleError("http://url", UnexpectedError("connect error")))
          titlesResponse.titles shouldBe Vector()
        }
      }
    }
  }
}
