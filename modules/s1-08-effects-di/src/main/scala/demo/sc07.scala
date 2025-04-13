package sc07

import scala.util.{Failure, Try}


object FromSlides:

  import cats.Monad
  import cats.syntax.all.*

  type UserId   = String
  type UserName = String
  type UserAge  = Int
  type UserInn  = String

  trait UserRepo[F[_]]:
    def getUserId(name: UserName): F[UserId]

  def UserRepo[F[_]](using inst: UserRepo[F]): UserRepo[F] =
    inst


  trait Esia[F[_]]:
    def getInn(userId: UserId): F[String]
    def getFullYears(userId: UserId): F[UserAge]

  def Esia[F[_]](using inst: Esia[F]): Esia[F] =
    inst


  def someLogic[F[_] : Esia : UserRepo : Monad](
    userName: UserName
  ): F[(UserAge, UserInn)] =
    for
      id  <- UserRepo.getUserId(userName)
      inn <- Esia.getInn(id)
      age <- Esia.getFullYears(id)
    yield (age, inn)

end FromSlides




// DI examples and explanations

case class Effect[+A](private[Effect] runSafe: () => Try[A]) {
  def runUnsafe(): A = runSafe().get
}

object Effect {

  def pure[A](a: => A): Effect[A] = Effect(() => Try(a))
  def fail[A](failure: Throwable): Effect[A] = Effect(() => Failure(failure))

  extension [A] (inOut: Effect[A])

    def map[B](f: A => B): Effect[B] =
      Effect(() => inOut.runSafe().map(f))

    def flatMap[B](f: A => Effect[B]): Effect[B] =
      Effect( () =>
        inOut.runSafe().fold(e => Effect.fail(e), f).runSafe()
      )

    def retry(f: Throwable => Effect[A]): Effect[A] =
      Effect( () =>
        inOut.runSafe().fold(f, success => Effect.pure(success)).runSafe()
      )

  trait Runtime:
    def eval[A](inOut: Effect[A]): Try[A] =
      inOut.runSafe()
}

// typeclass

trait Console[F[_]]:
  def printLine(line: => String): F[Unit]
  def readLine                  : F[String]

def Console[F[_]](using inst: Console[F]): Console[F] = inst

trait Random[F[_]]:
  def nextInt: F[Int]

def Random[F[_]](using inst: Random[F]): Random[F] = inst

trait Esia[F[_]]:
  def getInn(name: String): F[String]
  def getFullYears(name: String): F[Int]

def Esia[F[_]](using inst: Esia[F]): Esia[F] = inst


trait MonadError[F[_]]:
  def pure[A](a: A): F[A]
  def fail[A](failure: Throwable): F[A]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

object MonadErrorSyntax:

  def MonadError[F[_]](using ME: MonadError[F]): MonadError[F] = ME

  extension [A, F[_] : MonadError](fa: F[A])
    def map[B](f: A => B): F[B] =
      MonadError.map(fa)(f)
    def flatMap[B](f: A => F[B]): F[B] =
      summon[MonadError[F]].flatMap(fa)(f)

      
// Runtime


object EffectRuntime extends Effect.Runtime {

  given Console[Effect]:
    override def printLine(line: => String): Effect[Unit] =
      Effect.pure(println(line))
    override def readLine: Effect[String] =
      Effect.pure(scala.io.StdIn.readLine())

  given Random[Effect]:
    override def nextInt: Effect[Int] =
      Effect.pure(scala.util.Random().nextInt())

  given MonadError[Effect]:
    override def pure[A](a: A): Effect[A] = Effect.pure(a)
    override def fail[A](failure: Throwable): Effect[A] = Effect.fail(failure)
    override def map[A, B](fa: Effect[A])(f: A => B): Effect[B] = fa.map(f)
    override def flatMap[A, B](fa: Effect[A])(f: A => Effect[B]): Effect[B] = fa.flatMap(f)

}


object External {
  def newEffectfulFetcher: Esia[Effect] =
    new:
      override def getInn(name: String): Effect[String] =
        Effect.pure(name.length().toString*name.length())
      override def getFullYears(name: String): Effect[Int] =
        Effect.pure(name.length()+16)
}


// program


object Example extends App {
  import MonadErrorSyntax.*


  def greeting[F[_] : Console : Esia : MonadError]: F[String] =
    for 
      _       <- Console[F].printLine("Who are you?")
      name    <- Console[F].readLine
      _       <- Console[F].printLine("O!, Really?")
      confirm <- Console[F].readLine
      result  <- confirm.toLowerCase() match {
                  case "yes" => MonadError[F].pure(name)
                  case _     => MonadError[F].fail(new Exception("Inadequate"))
                }
      _       <- Console[F].printLine(s"Hello, $name!")
      userinn <- Esia[F].getInn(name)
      _       <- Console[F].printLine(s"Your inn is $userinn!")
    yield name

    // at the end of the world

  import EffectRuntime.given 
  given fetcher: Esia[Effect] = External.newEffectfulFetcher

  val program = greeting
  println(EffectRuntime.eval(program))
  println()
  println("One more time!")
  println()
  println("result = " + program.runUnsafe())

}


