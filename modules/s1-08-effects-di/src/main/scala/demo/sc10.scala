package sc10


object Runtime:

    // будем хранить "метки" имеющихся сервисов
    enum SvcTag[A]:
        case Service[Svc](name: String) extends SvcTag[Svc]

    // хранить метки будем в специальном типе-обёртке
    trait HasTag[A]:
        def tag: SvcTag[A]

    object HasTag:
        def apply[A](using H: HasTag[A]): HasTag[A] = H
        def svc[Svc](name: String): HasTag[Svc] = new HasTag[Svc]:
            def tag: SvcTag[Svc] =
                SvcTag.Service(name)


    // "копилка" для меток
    case class Env[+R](map: Map[SvcTag[?], Any]):
        def get[A >: R: HasTag]: A =
            map.getOrElse(HasTag[A].tag, throw new Exception("No such tag")).asInstanceOf[A]
        def add[A: HasTag](value: A): Env[A & R] =
            Env[A & R](map + (HasTag[A].tag -> value))

    object Env:
        val empty: Env[Any] = Env[Any](Map())

end Runtime
import Runtime.*


// Эффект
case class Effect[-R, +E, +A](run: Env[R] => Either[E, A])

object Effect:
    def pure[A](a: => A): Effect[Any, Nothing, A] =
        Effect(_ => Right(a))

    def raise[E](e: => E): Effect[Any, E, Nothing] =
        Effect(_ => Left(e))

    def service[S: HasTag]: Effect[S, Nothing, S] =
        Effect(env => Right(env.get[S]))


    // Операторы
    extension [R, E, A] (effect: Effect[R, E, A])
        def flatMap[R2 <: R, B](g: A => Effect[R2, E, B]): Effect[R2, E, B] =
            Effect( (env: Env[R2]) =>
                effect.run(env) match
                    case Right(a) => g(a).run(env)
                    case e => e.asInstanceOf
            )
        def map[B](g: A => B): Effect[R, E, B] =
            Effect[R, E, B]( (env: Env[R]) => 
                effect.run(env) match
                    case Right(a) => Right(g(a))
                    case e => e.asInstanceOf
        )
        def provide[R2 >: R, S: HasTag](value: S)(using ev: (S & R2) =:= R): Effect[R2, E, A] =
            Effect[R2, E, A](e => effect.run(ev.liftCo(e.add(value))))
end Effect
import Effect.*


// Example Services

// Services
trait InputSvc:
    def read: Effect[Any, Nothing, String]

trait OutputSvc:
    def write(data: String): Effect[Any, Nothing, Unit]


object StringInput:
    given HasTag[InputSvc] = HasTag.svc("InputSvc")
    def apply: InputSvc = new:
        override def read: Effect[Any, Nothing, String] =
            Effect.pure(scala.io.StdIn.readLine)

object StringOutput:
    given HasTag[OutputSvc] = HasTag.svc("OutputSvc")
    def apply: OutputSvc = new:
        override def write(data: String): Effect[Any, Nothing, Unit] =
            Effect.pure(println(data))



// --- Examples ---



// Business Domain & Logic
import StringInput.given
import StringOutput.given


case class User(name: String)

trait AuthService:
    def authenticate: Effect[InputSvc & OutputSvc, Throwable, User]

object AuthService {
    class Impl extends AuthService:
        override def authenticate: Effect[InputSvc & OutputSvc, Throwable, User] =
            for
                input   <- Effect.service[InputSvc]
                output  <- Effect.service[OutputSvc]
                _       <- output.write("Hello! What is your name?")
                name    <- input.read
                _       <- output.write("Really?")
                confirm <- input.read
                user    <- 
                            if (confirm.compareToIgnoreCase("yes") == 0)
                                Effect.pure(User(name))
                            else
                                Effect.raise(new Exception("Inadequate"))
            yield user
}


val prepared: Effect[InputSvc & OutputSvc, Throwable, User] = 
    new AuthService.Impl()
        .authenticate

val program =
    prepared
        .provide(StringOutput.apply)
        .provide(StringInput.apply)


@main
def test = 
    println("*"*16)
    println("DependencyControlled:")
    println()
    println(program.run(Env.empty))
    println()
    println("One more time")
    println()
    println(program.run(Env.empty))
    println() 


