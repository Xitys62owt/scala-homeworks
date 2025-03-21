import sbt.*

object deps {
  val munit = "org.scalameta" %% "munit" % "1.1.0" % Test
  val scalatest = "org.scalatest" %% "scalatest" % "3.2.19" % Test
  val cats = "org.typelevel" %% "cats-core" % "2.13.0"
}

object scalac {
  val v3 = "3.6.3"
}
