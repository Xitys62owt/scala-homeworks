package sc12

import cats.effect.*
import scala.concurrent.duration.*

// object OutcomeDescribed:
//     enum Outcome[F[_], E, A]:
//         case Succeeded[F[_], E, A](fa: F[A]) extends Outcome[F, E, A]
//         case Errored[F[_], E, A](e: E)       extends Outcome[F, E, A]
//         case Canceled[F[_], E, A]()          extends Outcome[F, E, A]
//     type OutcomeIO[A] = Outcome[IO, Throwable, A]
// end OutcomeDescribed

// val effect =
//   IO.unit
//     .map(_ => 1)
//     .flatMap(x => IO.apply(x.toString()))
//     .race(IO.never)
//     .both(IO.raiseError(new Exception("error")))
//     .orElse(IO.delay(println("other")).delayBy(5000.millis))
//     .as("discard and fail: ")
//     .flatMap(msg => IO.print(msg) >> IO.raiseWhen(1 > 0)(new Exception("Boom!")))
//     .handleErrorWith(err => IO.println(err.getMessage()))

object CeErrorsDemo extends IOApp:

  override def run(args: List[String]) =
    test
      .as(ExitCode.Success)

  def starter(ref: Ref[IO, Int]) =
    IO.race(ref.set(5), ref.update(_ - 1).delayBy(750.millis).foreverM.start)

  def reader(ref: Ref[IO, Int]): IO[Int] =
    ref.get
      .flatTap(current => IO.raiseWhen(current < 0)(new Exception(s"Boom on $current")))
      .handleErrorWith(th => IO.println(th.getMessage) *> IO(th.getMessage().drop(8).toInt))

  def scanner(readerIO: IO[Int]) =
    readerIO.flatTap(IO.println).delayBy(500.millis).iterateWhile(_ > -3)

  val test =
    for
      ref <- Ref[IO].of(0)
      _ <- starter(ref)
      _ <- IO.sleep(1.second)
      _ <- scanner(reader(ref))
    yield ()
