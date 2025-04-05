package hw02

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
        * Добавьте эффекту в исполняемой кодировке возможность восстанавливаться из ошибки
        */
        def recover(f: Throwable => A): Effect[A] =
            ???

        /**
        * Добавьте эффекту в исполняемой кодировке возможность восстанавливаться из ошибки
        */
        def recoverWith(f: Throwable => Effect[A]): Effect[A] =
            ???
