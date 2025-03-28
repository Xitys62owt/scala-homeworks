package sc02

import cats.effect.*
import scala.concurrent.duration.Duration


object CeUsingDemo extends IOApp:


  // override def run(args: List[String]) =
    // IO.println("Hello with Scala 3 using CE 3")
    // effect
    //   .as(ExitCode.Success)


  val effect =
    IO.unit
      .map(_ => 1)
      .flatMap(x => IO.apply(x.toString()))
      .race(IO.never)
      .both(IO.raiseError(new Exception("error")))
      .orElse(IO.delay(println("other")).delayBy(Duration.fromNanos(5000000L)))
      .as("discard and fail: ")
      .flatMap(msg => IO.print(msg) >> IO.raiseWhen(1 > 0)(new Exception("Boom!")))
      .handleErrorWith(err => IO.println(err.getMessage()))


  val helloProgram =
    for
      _       <- IO.println("Hello! What is your name?")
      name    <- IO.readLine
      _       <- IO.println("Really?")
      confirm <- IO.readLine
      _       <- IO.raiseWhen(
                  !confirm.equalsIgnoreCase("yes")
                )(
                  new Exception("Inadequate")
                )
    yield s"Hello, $name"

  override def run(args: List[String]) =
    helloProgram flatMap IO.println as ExitCode.Success



