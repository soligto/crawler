package crawler

import cats.effect.{ ContextShift, IO }
import distage.DIKey
import fs2.Stream
import izumi.distage.config.ConfigModuleDef
import izumi.distage.plugins.{ PluginConfig, PluginDef }
import izumi.distage.testkit.scalatest.{ AssertCIO, Spec1 }
import org.http4s.{ Request, Response }
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers

class Context(implicit val cs: ContextShift[IO])

abstract class Test extends Spec1[IO] with AssertCIO with Matchers {
  override def config =
    super.config.copy(
      configBaseName = "crawler",
      pluginConfig = Test.pluginConfig,
      memoizationRoots = Set(
        DIKey[Context]
      )
    )
}

object Test extends MockFactory {
  type Client[F[_]]          = Request[F] => Stream[F, Response[F]]
  type ServiceProvider[F[_]] = Client[F] => WebCrawlerService[F]

  val resourcesModule = new ConfigModuleDef {
    make[Context].from((cs: ContextShift[IO]) => new Context()(cs))
  }

  val servicesModule = new ConfigModuleDef {
    // мокирование не потокобезопасно, поэтому мок обёрнут в ресурс и снабжён семафором:
    // только один тест может работать с Client[IO] в один момент времени
    make[ServiceProvider[IO]].from { (contextShift: ContextShift[IO]) =>
      implicit val cs = contextShift
      (client: Client[IO]) => WebCrawlerService[IO](
        client,
        IO.pure(RegexTagParser("title")),
        ParserPipe.apply[IO, Tag]
      )
    }
  }

  object plugin extends PluginDef {
    include(resourcesModule)
    include(servicesModule)
  }

  val pluginConfig = PluginConfig.const(plugin)
}
