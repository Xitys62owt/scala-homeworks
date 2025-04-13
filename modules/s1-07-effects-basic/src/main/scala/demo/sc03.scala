package sc03

import zio.*

object ZioUsingDemo extends ZIOAppDefault:

  // override def run =
  //   ZIO.succeed(println("Hello with Scala 3 using ZIO 2"))
  //     .exitCode

  val effect =
    ZIO.unit
      .map(_ => 1)
      .flatMap(x => ZIO.succeed(x.toString()))
      .race(ZIO.never)
      .zip(ZIO.fail(new Exception("error")))
      .orElse(ZIO.succeed(println("other")).delay(Duration.fromMillis(5000L)))
      .as("discard and fail: ")
      .flatMap(msg => ZIO.consoleWith(_.printLine(msg)) *> ZIO.when(1 > 0)(ZIO.fail(new Exception("Boom!"))))
      .catchAll(err => ZIO.logInfo(err.getMessage()))

  val helloProgram: ZIO[Any, Exception, String] =
    for
      _ <- Console.printLine("Hello! What is your name?")
      name <- Console.readLine
      _ <- Console.printLine("Really?")
      confirm <- Console.readLine
      _ <- ZIO.when(
        !confirm.equalsIgnoreCase("yes")
      )(
        ZIO.fail(new Exception("Inadequate"))
      )
    yield s"Hello, $name"

  override val run =
    helloProgram.flatMap(str => Console.printLine(str))
