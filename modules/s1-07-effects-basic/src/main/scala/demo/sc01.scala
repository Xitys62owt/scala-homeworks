package sc01

import scala.util.{Failure, Success, Try}



// Handmade effect system





////////////////////////////////////////////////////////////////////
// Executable encoding

object ExecutableDemo1: 

  final case class Effect[A](run: () => A)

  object Effect:
    def pure[A](value: => A) = Effect(() => value)

  extension [A] (fa: Effect[A])
    def flatMap[B](f: A => Effect[B]) = Effect(() => f(fa.run()).run())
    def map[B](f: A => B) = Effect(() => f(fa.run()))


@main
def execDemo1 =
  import ExecutableDemo1.*
  @annotation.tailrec
  def fact(limit: Int, acc: Effect[BigInt] = Effect.pure(1)): Effect[BigInt] =
    if (limit < 1)
      acc.map {
        a => 
          println("bottom")
          a
      }
    else
      fact(limit - 1, acc.map(_ * limit) )

  val calculation = fact(100)
  println(calculation.run())
  println(calculation.run())

  // Ленивость есть, а где контроль ошибок?








object ExecutableDemo2: 

  case class Effect[A](safeRun: () => Try[A])
    

  object Effect:
    def pure[A](value:  => A)         = Effect(() => Try(value))
    def raise[A](error: => Throwable) = Effect[A](() => Failure(error))

  extension [A] (fa: Effect[A])
    def run: () => A = () => fa.safeRun().get

    def flatMap[B](f: A => Effect[B]) = Effect(() =>
      fa.safeRun() match
        case Failure(exception) => Failure(exception)
        case Success(value)     => f(value).safeRun()
      )

    def map[B](f: A => B) = Effect(() => fa.safeRun().map(f))




@main
def execDemo2 =
  import ExecutableDemo2.*
  def fact(limit: Int, acc: BigInt = 1): Effect[BigInt] =
    if (limit <= 0)
      Effect.raise(new Exception("Boom!"))
    else if (limit == 1)
      println("bottom")
      Effect.pure(acc)
    else
      fact(limit - 1, acc * limit)

  println(fact(100).safeRun())
  println(fact(-10).safeRun())



@main
def helloDemo =
  import ExecutableDemo2.*
  val program =
    for {
      _       <- Effect.pure(println("Who are you?"))
      name    <- Effect.pure(scala.io.StdIn.readLine())
      _       <- Effect.pure(println("O!, Really?"))
      confirm <- Effect.pure(scala.io.StdIn.readLine())
      result  <- confirm.toLowerCase() match {
                  case "yes" => Effect.pure(name)
                  case _     => Effect.raise(new Exception("Inadequate"))
                }
      _       <- Effect.pure(println(s"Hello, $name!"))
    } yield result


  println(program.run())
  println()
  println("One more time!")
  println()
  println(program.safeRun())



  

////////////////////////////////////////////////////////////////////
// Declarative encoding



// Упрощённый знакомый пример:
enum Eval[A]:
  case Delay(f: () => Eval[A])              extends Eval[A]
  case Value(v: A)                          extends Eval[A]
  case FlatMap(e: Eval[A], f: A => Eval[A]) extends Eval[A]

  def map(f: A => A): Eval[A] =
    flatMap(((a: A) => Delay(() => Value(f(a)))))
  def flatMap(f: A => Eval[A]): Eval[A] =
    this match
      case Eval.Delay(_)      => FlatMap(this, f)
      case Eval.Value(v)      => Delay(() => f(v))
      case Eval.FlatMap(e, g) => FlatMap(e, x => g(x).flatMap(f))

object Eval:
	def delay[A](v: => Eval[A]): Eval[A] = v
	def value[A](v: A): Eval[A] = Eval.Value(v)
	@annotation.tailrec 
	def fold[A](ev: Eval[A]): A = ev match
		case Eval.Delay(f)      => fold(f())
		case Eval.Value(v)      => v
		case Eval.FlatMap(e, f) => e match
			case Eval.Delay(g)      => fold(g().flatMap(f))
			case Eval.Value(v)      => fold(f(v))
			case Eval.FlatMap(e, g) => fold(e.flatMap(x => g(x).flatMap(f)))
// Ленивость есть, а где контроль ошибок?






/**
 * Демонстрационный(гибридный) вариант.
 * В основе декларативный подход, но смешанный с исполняемым.
 * Из-за смешения сломана ленивость.
 * В дальнейшем мы ее починим применяя всё этот же InOut.
 */
object InOutSystem {

  // 1. domain

  sealed trait InOut[+E, +A] { self =>

    override def toString: String =
      self match {
        case InOutSuccess(eval) =>
          s"InOutSuccess(${eval()})"
        case InOutFailure(err) =>
          s"InOutFailure(${err.toString()})"
        case sc01.InOutSystem.Map(parent, f) =>
          parent match
            case InOutSuccess(value) =>
              s"InOutSuccess(${f(value())})"
            case InOutFailure(error) =>
              s"InOutFailure(${error.toString()})"
            case Map(inOut, g) =>
              inOut.map(x => f(g(x))).toString
            case FlatMap(inOut, g) =>
              inOut.flatMap(x => g(x).map(f)).toString
        case sc01.InOutSystem.FlatMap(parent, f) =>
          parent match
            case InOutSuccess(value) =>
              s"InOutSuccess(${f(value())})"
            case InOutFailure(error) =>
              s"InOutFailure(${error.toString()})"
            case Map(inOut, g) =>
              inOut.map(x => f(g(x))).toString
            case FlatMap(inOut, g) =>
              inOut.flatMap(x => g(x).map(f)).toString 
      }

  }

  private[InOutSystem] final case class InOutSuccess[A](value: () => A)
    extends InOut[Nothing, A]

  private[InOutSystem] final case class InOutFailure[E](error: E)
    extends InOut[E, Nothing]

  private[InOutSystem] final case class Map[+E, A, +B](
    inOut:  InOut[E, A],
    f: A => B
  ) extends InOut[E, B]

  private[InOutSystem] final case class FlatMap[+E, A, +B](
    inOut:  InOut[E, A],
    f: A => InOut[E, B]
  ) extends InOut[E, B]


  // 2. constructors

  object InOut {
    def pure[A](value: => A): InOut[Nothing, A] =
      InOutSuccess(() => value)

    def raise[E](error: => E): InOut[E, Nothing] =
      InOutFailure(error)

    def apply[A](calc: => A): InOut[Throwable, A] =
      pure(Try(calc)).flatMap(_.fold(err => raise(err), res => pure(res)))
  }


  // 3. operators

  extension [E, A](inOut: InOut[E, A])

   def map[B](f: A => B): InOut[E, B] =
     Map[E, A, B](inOut, f)

   def flatMap[B](f: A => InOut[E, B]): InOut[E, B] =
     FlatMap[E, A, B](inOut, f)

    // def flatMap[B](f: A => InOut[E, B]): InOut[E, B] =
    //   inOut match {
    //     case InOutSuccess(value) =>
    //       f(value())
    //     case failure: InOutFailure[?] =>
    //       failure
    //   }

    // def map[B](f: A => B): InOut[E, B] =
    //   flatMap(a => InOut.pure(f(a)))


}


object InOutWithErrorDemo extends App {
  import InOutSystem._

  val program: InOut[Throwable, String] =
    for {
      _       <- InOut(println("Who are you?"))
      name    <- InOut(scala.io.StdIn.readLine())
      _       <- InOut(println("O!, Really?"))
      confirm <- InOut(scala.io.StdIn.readLine())
      result  <- confirm.toLowerCase() match {
                  case "yes" => InOut.pure(name)
                  case _     => InOut.raise(new Exception("Inadequate"))
                }
      _       <- InOut(println(s"Hello, $name!"))
    } yield result


  println(program.toString)
  println()
  println("One more time!")
  println()
  println(program.toString)

}






