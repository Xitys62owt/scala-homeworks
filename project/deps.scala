import sbt.*

object deps {
  val munit     = "org.scalameta" %% "munit"     % "1.1.0"  % Test
  val scalatest = "org.scalatest" %% "scalatest" % "3.2.19" % Test
  val scalamock = "org.scalamock" %% "scalamock" % "7.1.0"  % Test
  val cats      = "org.typelevel" %% "cats-core" % "2.13.0"

  val ce     = "org.typelevel" %% "cats-effect"                   % "3.5.7"
  val cetest = "org.typelevel" %% "cats-effect-testing-scalatest" % "1.5.0" % Test

  val zio     = "dev.zio" %% "zio"      % "2.1.16"
  val ziotest = "dev.zio" %% "zio-test" % "2.1.16" % Test

}

object scalac {
  val v3 = "3.6.3"
}
