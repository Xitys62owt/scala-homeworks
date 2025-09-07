package sc08

import izumi.reflect.Tag

import scala.util.Try

////////////////////////////////////
//----------------------------------
// DI
//----------------------------------
////////////////////////////////////
object InOutSystem {

  sealed trait InOut[+E, +A] { self =>
    override def toString: String =
      self match
        case InOutSuccess(eval) => s"InOutSuccess(${eval()})"
        case InOutFailure(err)  => s"InOutFailure(${err.toString()})"
        case sc08.InOutSystem.Map(parent, f) =>
          parent match
            case InOutSuccess(eval)  => s"InOutSuccess(${f(eval())})"
            case InOutFailure(error) => s"InOutFailure(${error.toString()})"
            case Map(inOut, g)       => inOut.map(a => InOutSuccess(() => f(g(a)))).toString
            case FlatMap(inOut, g)   => inOut.flatMap(a => g(a).map(f)).toString
        case sc08.InOutSystem.FlatMap(parent, f) =>
          parent match
            case InOutSuccess(value) => f(value()).toString
            case InOutFailure(error) => s"InOutFailure(${error.toString()})"
            case Map(inOut, g)       => inOut.flatMap(a => f(g(a))).toString
            case FlatMap(inOut, g)   => inOut.flatMap(a => g(a).flatMap(f)).toString
  }

  private[InOutSystem] case class InOutSuccess[A](value: () => A) extends InOut[Nothing, A]
  private[InOutSystem] case class InOutFailure[E](error: E) extends InOut[E, Nothing]
  private[InOutSystem] case class Map[+E, A, +B](inOut: InOut[E, A], f: A => B) extends InOut[E, B]
  private[InOutSystem] case class FlatMap[+E, A, +B](inOut: InOut[E, A], f: A => InOut[E, B]) extends InOut[E, B]

  object InOut {
    def pure[A](value: => A): InOut[Nothing, A] = InOutSuccess(() => value)
    def raise[E](error: => E): InOut[E, Nothing] = InOutFailure(error)
    def apply[A](calc: => A): InOut[Throwable, A] = pure(Try(calc)).flatMap(_.fold(err => raise(err), res => pure(res)))

  }

  extension [E, A](inOut: InOut[E, A])
    def map[B](f: A => B): InOut[E, B] = Map[E, A, B](inOut, f)
    def flatMap[B](f: A => InOut[E, B]): InOut[E, B] = FlatMap[E, A, B](inOut, f)

}

import sc08.InOutSystem.*

// R => F[ E | A ]
trait InOutProgram[-R, +E, +A] extends (R => InOut[E, A])

////////////////////////////////////
//----------------------------------
// Some services

trait InOutConsole {
  def printLine(line: => String): InOut[Throwable, Unit]
  def readLine: InOut[Throwable, String]
}

trait InOutRandom:
  def nextInt: InOut[Nothing, Int]

trait Esia:
  def getInn(userId: Int): InOut[Throwable, String]
  def getAge(userId: Int): InOut[Throwable, Int]

trait Users:
  def getId(userName: String): InOut[Throwable, Int]
  def getName(userId: Int): InOut[Throwable, String]

//----------------------------------
////////////////////////////////////
// Some instances
//----------------------------------

trait Console extends InOutConsole {
  override def printLine(line: => String): InOut[Throwable, Unit] =
    InOut.pure(println(line))
  override def readLine: InOut[Throwable, String] =
    InOut(scala.io.StdIn.readLine())
}
object DefaultConsole extends Console

trait Random extends InOutRandom {
  override def nextInt: InOut[Nothing, Int] =
    InOut.pure(scala.util.Random().nextInt())
}
object DefaultRandom extends Random

trait EsiaImpl extends Esia:
  override def getInn(userId: Int): InOut[Throwable, String] =
    InOut.pure(userId.toString * userId)
  override def getAge(userId: Int): InOut[Throwable, Int] =
    InOut.pure(userId + 16)

trait UsersImpl extends Users:
  override def getId(userName: String): InOut[Throwable, Int] =
    InOut.pure(userName.length())
  override def getName(userId: Int): InOut[Throwable, String] =
    InOut.pure(userId.toString * userId)

//----------------------------------
////////////////////////////////////

////////////////////////////////////
//----------------------------------
// Example 1
//----------------------------------
////////////////////////////////////

object InOutWithDeps1Demo extends App {

  val program: InOutProgram[InOutConsole & Esia & Users, Throwable, String] =
    new:
      override def apply(consoleAndFetcher: InOutConsole & Esia & Users): InOut[Throwable, String] =
        for {
          _ <- consoleAndFetcher.printLine("Who are you?")
          name <- consoleAndFetcher.readLine
          _ <- consoleAndFetcher.printLine(("O!, Really?"))
          confirm <- consoleAndFetcher.readLine
          result <- confirm.toLowerCase() match {
            case "yes" => InOut.pure(name)
            case _     => InOut.raise(new Exception("Inadequate"))
          }
          _ <- consoleAndFetcher.printLine(s"Hello, $name!")
          uid <- consoleAndFetcher.getId(name)
          inn <- consoleAndFetcher.getInn(uid)
          _ <- consoleAndFetcher.printLine(s"Your inn is $inn!")
        } yield result

  // Есть гарантии достаточности R, но очень неудобно R формировать
  val deps = new EsiaImpl with UsersImpl with Console

  val eval = program(deps)
  println(eval.toString)
  println()
  println("One more time!")
  println()
  println(eval.toString)

}

////////////////////////////////////
//----------------------------------
// Example 2
//----------------------------------
////////////////////////////////////

////////////////////////////////////
// Runtime
//----------------------------------

class Runtime(map: Map[String, Any] = Map.empty) {

  def provide[S: Tag](service: S): Runtime =
    Runtime(map + (Tag[S].tag.repr -> service))

  def provide[S1: Tag, S2: Tag](service1: S1, service2: S2): Runtime =
    Runtime(map ++ Map(Tag[S1].tag.repr -> service1, Tag[S2].tag.repr -> service2))

  def provide[S1: Tag, S2: Tag, S3: Tag](service1: S1, service2: S2, service3: S3): Runtime =
    Runtime(map ++ Map(Tag[S1].tag.repr -> service1, Tag[S2].tag.repr -> service2, Tag[S3].tag.repr -> service3))

  def service[S: Tag]: InOut[Throwable, S] =
    InOut(map(Tag[S].tag.repr).asInstanceOf[S])
}

object Runtime:
  def default: Runtime =
    Runtime(Map.empty)

  def service[S: Tag](using runtime: Runtime): InOut[Throwable, S] =
    runtime.service[S]

  def serviceWith[S: Tag, A](f: S => A)(using runtime: Runtime): InOut[Throwable, A] =
    runtime.service[S].map(f)

  def serviceWithIO[S: Tag](using runtime: Runtime): ServiceWithIoPartiallyApplied[S] =
    ServiceWithIoPartiallyApplied(service[S])

  class ServiceWithIoPartiallyApplied[S](service: InOut[Throwable, S]):
    def apply[A](f: S => InOut[Throwable, A]): InOut[Throwable, A] =
      service.flatMap(f)

//----------------------------------
////////////////////////////////////

object InOutWithDeps2Demo extends App {

  val program: InOutProgram[Runtime, Throwable, String] =
    new:
      override def apply(using runtime: Runtime): InOut[Throwable, String] =
        for {
          console <- Runtime.service[InOutConsole]
          _ <- console.printLine("Who are you?")
          name <- console.readLine
          _ <- console.printLine(("O!, Really?"))
          confirm <- console.readLine
          result <- confirm.toLowerCase() match {
            case "yes" => InOut.pure(name)
            case _     => InOut.raise(new Exception("Inadequate"))
          }
          _ <- console.printLine(s"Hello, $name!")
          userId <- Runtime.serviceWithIO[Users](_.getId(name))
          userInn <- Runtime.serviceWithIO[Esia](_.getInn(userId))
          _ <- console.printLine(s"Your inn is $userInn!")
        } yield result

  // Формировать R удобнее, но очень слабый контроль над этим каналом
  given runtime: Runtime =
    Runtime.default
      .provide(
        new EsiaImpl {}: Esia,
        new UsersImpl {}: Users,
        DefaultConsole: InOutConsole
      )

  val eval = program.apply(runtime)
  println(eval.toString)
  println()
  println("Once again!")
  println()
  println(eval.toString())

}
