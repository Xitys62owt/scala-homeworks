package sc13

import zio.* 
import zio.Duration.*

// val effect =
//   ZIO.unit
//     .map(_ => 1)
//     .flatMap(x => ZIO.succeed(x.toString()))
//     .race(ZIO.never)
//     .zip(ZIO.fail(new Exception("error")))
//     .orElse(ZIO.succeed(println("other")).delay(Duration.fromMillis(5000L)))
//     .as("discard and fail: ")
//     .flatMap(msg => ZIO.consoleWith(_.printLine(msg)) *> ZIO.when(1 > 0)(ZIO.fail(new Exception("Boom!"))))
//     .catchAll(err => ZIO.logInfo(err.getMessage()))


object ZioErrorsDemo extends ZIOAppDefault:

  override def run =
    test
      .catchAllCause(cause => ZIO.logErrorCause("Run failed", cause))
      .exitCode

  def starter(ref: Ref[Int]) =
    ZIO.raceFirst(ref.set(5), Seq(ref.update(_ - 1).delay(750.millis).scheduleFork(Schedule.forever).unit))

  def reader(ref: Ref[Int]) =
    ref.get
      .tap(current => ZIO.when(current < 0)(ZIO.fail((s"Boom on $current"))))
      .catchAll( err =>
        ZIO.succeed(err.drop(8).toInt)
        )

  def scanner(readerIO: Task[Int]) =
    readerIO.tap(number => Console.printLine(number)).delay(500.millis).repeatWhile(_ > -3)


  val test = 
    for 
      ref   <- Ref.make(0)
      _     <- starter(ref)
      _     <- ZIO.sleep(1.second)
      _     <- scanner(reader(ref))
    yield ()


