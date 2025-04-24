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

val s1_03 = "s1-03-collections"
lazy val `s1-03-collections` = (project in file(s"modules/$s1_03"))
  .settings(commonSettings)
  .settings(
    name := s1_03,
    libraryDependencies ++= Seq(
      munit
    )
  )
val s1_04 = "s1-04-laziness-errors"

lazy val `s1-04-laziness-errors` = (project in file(s"modules/$s1_04"))
  .settings(commonSettings)
  .settings(
    name := s1_04,
    libraryDependencies ++= Seq(
      munit
    )
  )

val s1_05 = "s1-05-type-classes"

lazy val `s1-05-type-classes` = (project in file(s"modules/$s1_05"))
  .settings(commonSettings)
  .settings(
    name := s1_05,
    libraryDependencies ++= Seq(
      cats,
      munit
    )
  )

val s1_06 = "s1-06-concurrency"

lazy val `s1-06-concurrency` = (project in file(s"modules/$s1_06"))
  .settings(commonSettings)
  .settings(
    name := s1_06,
    libraryDependencies ++= Seq(
      scalatest
    )
  )

val s1_07 = "s1-07-effects-basic"

lazy val `s1-07-effects-basic` = (project in file(s"modules/$s1_07"))
  .settings(commonSettings)
  .settings(
    name := s1_07,
    libraryDependencies ++= Seq(
      scalatest, ce, cetest, zio, ziotest
    )
  )

val s1_08 = "s1-08-effects-di"

lazy val `s1-08-effects-di` = (project in file(s"modules/$s1_08"))
  .settings(commonSettings)
  .settings(
    name := s1_08,
    libraryDependencies ++= Seq(
      scalatest, ce, cetest, zio, ziotest
    )
  )

val s1_09 = "s1-09-functor-flatmap"

lazy val `s1-09-functor-flatmap` = (project in file(s"modules/$s1_09"))
  .settings(commonSettings)
  .settings(
    name := s1_09,
    libraryDependencies ++= Seq(
      scalatest, ce, cetest, zio, ziotest
    )
  )

val s1_10 = "s1-10-monad-errors"

lazy val `s1-10-monad-errors` = (project in file(s"modules/$s1_10"))
  .settings(commonSettings)
  .settings(
    name := s1_10,
    libraryDependencies ++= Seq(
      scalatest, ce, cetest, zio, ziotest
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
    `s1-03-collections`,
    `s1-04-laziness-errors`,
    `s1-05-type-classes`,
    `s1-06-concurrency`,
    `s1-07-effects-basic`,
    `s1-08-effects-di`,
    `s1-09-functor-flatmap`,
    `s1-10-monad-errors`,
  )

lazy val moduleKeys: Map[String, String] =
  List(
    s1_01,
    s1_02,
    s1_03,
    s1_04,
    s1_05,
    s1_06,
    s1_07,
    s1_08,
    s1_09,
  ).map(x => x.take(5) -> x).toMap

commands += Command.command("hw") { state =>
  val branch =
    sys.env.get("CI_COMMIT_BRANCH").orElse(Process("git branch --show-current").lineStream.headOption)

  sLog.value.warn(s"CURRENT BRANCH: $branch")

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
