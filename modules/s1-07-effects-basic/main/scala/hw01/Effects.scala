package hw01

import scala.util.{Failure, Success, Try}

object Executable:

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

        /**
          * Напишите метод, который приняв на вход другой эффект породит новый эффект такой, что:
          * * Если результаты исполнения обоих эффектов (и исходного и переданного) будут успешными, то новый эффект при исполнении должен завешаться успехом с кортежем результатов обоих эффектов
          * * Если первый эффект завершится ошибкой, то надо вернуть новый эффект, при исполнении который будет завершаться ошибкой певого вычисления
          * * Если первый завершится успехом, а второй ошибкой, тогда надо вернуть новый эффект, который при исполнении будет завершаться ошибкой второго эффекта
          *
          * @param B тип результата переданного методу эффекта
          * @param other эффект переданный методы, завершающийся при исполнении объектом типа B или ошибкой
          * @return эффект для Tuple2[A, B] или ошибка
          */
        def both[B](other: Effect[B]): Effect[(A, B)] =
            ???

        
object Declarative:

    sealed trait Effect[+E, +A]
    sealed trait EffectResult[+E, +A]

    case class EffectSuccess[+A](value: A) extends Effect[Nothing, A] with EffectResult[Nothing, A]
    case class EffectFailure[+E](error: E) extends Effect[E, Nothing] with EffectResult[E, Nothing]
    case class Delay[+E, +A](dalayed: () => Effect[E, A]) extends Effect[E, A]
    case class FlatMap[+E, A, +B](parent:  Effect[E, A], f: A => Effect[E, B]) extends Effect[E, B]

    extension [E, A](effect: Effect[E, A])
        def flatMap[B](f: A => Effect[E, B]): Effect[E, B] =
            effect match
                case EffectSuccess(value) =>
                    Delay(() => f(value))
                case failure: EffectFailure[?] =>
                    failure
                case Delay(_) =>
                    FlatMap(effect, f)
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
          * Напишите метод, который приняв на вход другой эффект породит новый эффект такой, что:
          * * Если результаты исполнения обоих эффектов (и исходного и переданного) будут успешными, то новый эффект при исполнении должен завешаться успехом с кортежем результатов обоих эффектов
          * * Если первый эффект завершится ошибкой, то надо вернуть новый эффект, при исполнении который будет завершаться ошибкой певого вычисления
          * * Если первый завершится успехом, а второй ошибкой, тогда надо вернуть новый эффект, который при исполнении будет завершаться ошибкой второго эффекта
          *
          * @param B тип результата переданного методу эффекта
          * @param other эффект переданный методы, завершающийся при исполнении объектом типа B или ошибкой
          * @return эффект для Tuple2[A, B] или ошибка
          */
        def zip[B](other: Effect[E, B]): Effect[E, (A, B)] =
            ???


    object Effect:
        def pure[A](value: => A): Effect[Nothing, A] =
            Delay(() => EffectSuccess(value))

        def raise[E](error: E): Effect[E, Nothing] =
            EffectFailure(error)

        def apply[A](calc: => A): Effect[Throwable, A] =
            pure(Try(calc)).flatMap(_.fold(err => raise(err), res => EffectSuccess(res)))
                    
