name := "crawler"

version := "0.1"

scalaVersion := "2.13.5"

val V = new {
  val catsCore   = "2.4.2"
  val catsEffect = "2.3.3"
  val circe      = "0.13.0"
  val distage    = "1.0.0"
  val http4s     = "0.21.0"
  val fs2        = "2.5.0"
  val aalto      = "1.2.2"
  val scalamock  = "5.1.0"
}

lazy val cats = Seq(
  "org.typelevel" %% "cats-core"   % V.catsCore,
  "org.typelevel" %% "cats-effect" % V.catsEffect,
  "org.typelevel" %% "cats-free"   % V.catsCore
)

lazy val circe = Seq(
  "io.circe" %% "circe-core"    % V.circe,
  "io.circe" %% "circe-generic" % V.circe,
  "io.circe" %% "circe-parser"  % V.circe
)

lazy val http4s = Seq(
  "org.http4s" %% "http4s-dsl"          % V.http4s,
  "org.http4s" %% "http4s-blaze-client" % V.http4s,
  "org.http4s" %% "http4s-blaze-server" % V.http4s,
  "org.http4s" %% "http4s-circe"        % V.http4s,
  "org.http4s" %% "http4s-core"         % V.http4s
)

lazy val fs2 = Seq(
  "co.fs2" %% "fs2-core" % V.fs2
)

lazy val aalto = Seq(
  "com.fasterxml" % "aalto-xml" % V.aalto
)

lazy val distage = Seq(
  "io.7mind.izumi" %% "distage-core"              % V.distage,
  "io.7mind.izumi" %% "distage-extension-config"  % V.distage,
  "io.7mind.izumi" %% "distage-framework"         % V.distage,
  "io.7mind.izumi" %% "distage-testkit-scalatest" % V.distage,
  "io.7mind.izumi" %% "logstage-adapter-slf4j"    % V.distage
)

lazy val scalamock = "org.scalamock" %% "scalamock" % V.scalamock % Test

libraryDependencies ++= (
  cats ++ circe ++ http4s ++ fs2 ++ aalto ++ distage ++ Seq(scalamock)
)
