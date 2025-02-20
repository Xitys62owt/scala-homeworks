import org.typelevel.scalacoptions.{ScalaVersion, ScalacOption, ScalacOptions}
import deps.*
import sbt.internal.*
import StateSyntax.*
import sbt.Project.projectToRef

import scala.Ordering.Implicits.*

import scala.sys.process.Process

ThisBuild / organization     := "T-Bank"
ThisBuild / scalaVersion     := scalac.v3

lazy val commonSettings = Seq(
  Test / tpolecatExcludeOptions ++= Set(
    ScalacOptions.warnUnusedLocals,
    ScalacOptions.fatalWarnings,
    ScalacOptions.privateWarnUnusedLocals
  ),
  publish / skip := true
)

val s1_01 = "s1-01-scala-intro"
lazy val `s1-01-scala-intro` = (project in file(s"modules/$s1_01"))
  .settings(commonSettings)
  .settings(
    name := s1_01,
    libraryDependencies ++= Seq(
      munit
    )
  )

val s1_02 = "s1-02-adts"
lazy val `s1-02-adts` = (project in file(s"modules/$s1_02"))
  .settings(commonSettings)
  .settings(
    name := s1_02,
    libraryDependencies ++= Seq(
      munit
    )
  )

lazy val `root` = (project in file("."))
  .settings(
    name           := "root",
    publish / skip := true
  )
  .aggregate(
    `s1-01-scala-intro`,
    `s1-02-adts`,
  )

lazy val moduleKeys: Map[String, String] =
  List(
    s1_01,
    s1_02
  ).map(x => x.take(5) -> x).toMap

commands += Command.command("hw") { state =>
  val branch = Process("git branch --show-current").lineStream.headOption

  val pattern = """solution-(s\d-\d\d).*""".r
  branch.flatMap {
    case pattern(x) =>
      val key = moduleKeys.get(x)
      if (key.isEmpty)
        sLog.value.warn(s"WARN: Current branch starts with 'solution-' prefix, but $x doesn't correspond to any module")
      key
    case _ => None
  }.fold(
    runCommand("test", state)
  )(m => runCommand(s"$m / scalafmtCheck; $m / test", state))
}
