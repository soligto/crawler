package crawler

import cats.effect.concurrent.Semaphore
import cats.effect.{ ContextShift, IO, Resource }
import distage.DIKey
import izumi.distage.config.ConfigModuleDef
import izumi.distage.plugins.{ PluginConfig, PluginDef }
import izumi.distage.testkit.scalatest.{ AssertCIO, Spec1 }
import org.http4s.client.Client
import org.scalamock.scalatest.MockFactory
import org.scalatest.matchers.should.Matchers

class Context(implicit val cs: ContextShift[IO])

abstract class Test extends Spec1[IO] with AssertCIO with Matchers {
  override def config =
    super.config.copy(
      configBaseName = "crawler",
      pluginConfig = Test.pluginConfig,
      memoizationRoots = Set(
        DIKey[Context],
        DIKey[MockSemaphore]
      )
    )
}

object Test extends MockFactory {
  val resourcesModule = new ConfigModuleDef {
    make[Context].from((cs: ContextShift[IO]) => new Context()(cs))
  }

  val servicesModule = new ConfigModuleDef {
    // мокирование не потокобезопасно, поэтому мок обёрнут в ресурс и снабжён семафором:
    // только один тест может работать с Client[IO] в один момент времени
    make[MockSemaphore].fromEffect(MockSemaphore.apply(_: ContextShift[IO]))
    make[Client[IO]].fromResource { (context: Context, semaphore: MockSemaphore) =>
      import context._
      Resource.make {
        semaphore.instance.acquire *> IO.pure(mock[Client[IO]])
      } { _ =>
        semaphore.instance.release
      }
    }
  }

  object plugin extends PluginDef {
    include(resourcesModule)
    include(servicesModule)
  }

  val pluginConfig = PluginConfig.const(plugin)
}

case class MockSemaphore(instance: Semaphore[IO])
object MockSemaphore {
  def apply(implicit cs: ContextShift[IO]): IO[MockSemaphore] = {
    Semaphore[IO](1).map(s => MockSemaphore(s))
  }
}
