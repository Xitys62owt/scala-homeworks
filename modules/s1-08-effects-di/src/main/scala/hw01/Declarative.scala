package hw01

import scala.util.Try

object Declarative:

  sealed trait Effect[+E, +A]
  sealed trait EffectResult[+E, +A]

  case class EffectSuccess[+A](value: A) extends Effect[Nothing, A] with EffectResult[Nothing, A]
  case class EffectFailure[+E](error: E) extends Effect[E, Nothing] with EffectResult[E, Nothing]
  case class Delay[+E, +A](dalayed: () => Effect[E, A]) extends Effect[E, A]
  case class FlatMap[+E, A, +B](parent: Effect[E, A], f: A => Effect[E, B]) extends Effect[E, B]
  // case class Recover[E, +A](delayed: Effect[E, A], recover: E => Effect[E, A]) extends Effect[E, A]

  extension [E, A](effect: Effect[E, A])
    def flatMap[B](f: A => Effect[E, B]): Effect[E, B] =
      effect match
        case EffectSuccess(value) =>
          Delay(() => f(value))
        case failure: EffectFailure[?] =>
          failure
        case Delay(x) =>
          FlatMap(x(), f)
        case FlatMap(parent, g) =>
          FlatMap(parent, x => g(x).flatMap(f))

    def map[B](f: A => B): Effect[E, B] =
      effect.flatMap(a => EffectSuccess(f(a)))

    def eval: EffectResult[E, A] =
      effect match
        case EffectSuccess(value) =>
          EffectSuccess(value)
        case EffectFailure(error) =>
          EffectFailure(error)
        case Delay(dalayed) =>
          dalayed().eval
        case FlatMap(parent, f) =>
          parent match
            case EffectSuccess(value) =>
              f(value).eval
            case EffectFailure(error) =>
              EffectFailure(error)
            case Delay(dalayed) =>
              dalayed().flatMap(f).eval
            case FlatMap(inner, g) =>
              inner.flatMap(x => g(x).flatMap(f)).eval

    /**
        * Добавьте эффекту в декларативной кодировке возможность восстанавливаться из ошибки
        */
    def recover(f: E => A): Effect[Nothing, A] =
      ???

    /**
        * Добавьте эффекту в декларативной кодировке возможность восстанавливаться из ошибки
        */
    def recoverWith(f: E => Effect[E, A]): Effect[E, A] =
      ???

  object Effect:
    def pure[A](value: => A): Effect[Nothing, A] =
      Delay(() => EffectSuccess(value))

    def raise[E](error: E): Effect[E, Nothing] =
      EffectFailure(error)

    def apply[A](calc: => A): Effect[Throwable, A] =
      pure(Try(calc)).flatMap(_.fold(err => raise(err), res => EffectSuccess(res)))
