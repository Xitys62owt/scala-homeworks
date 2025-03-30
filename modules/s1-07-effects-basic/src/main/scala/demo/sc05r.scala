package sc05

import cats.effect.*
import cats.effect.kernel.Ref
import cats.syntax.all.*
import scala.concurrent.duration.*


object CeRefDescribed:
    //cats.effect.Ref - функциональная обертка над AtomicReference:
    abstract class Ref[F[_], A]:
        def get: F[A]
        def set(a: A): F[Unit]
        def modify[B](f: A => (A, B)): F[B]
        def update(f: A => A): F[Unit]
        def tryUpdate(f: A => A): F[Boolean]
        def tryModify[B](f: A => (A, B)): F[Option[B]]
        /// и другие методы
        

object CeRefDemo extends IOApp:

    def registerMessage(message: String, ref: Ref[IO, List[String]]): IO[Unit] =
        ref.update((list: List[String]) => message :: list)

    def readIncomingQueue(latency: Duration, message: String) =
        IO(message).delayBy(latency)

    val register = for
        ref <- Ref[IO].of(List.empty[String])
        _   <- IO.parTraverseN(16)((1 to 10).toList):
                idx => 
                    readIncomingQueue((17 % idx)*100.millis, s"Recieved $idx message")
                        .flatMap(msg => registerMessage(msg, ref))
        reg <- ref.get
    yield reg


    override def run(args: List[String]) =
        register
        .flatMap(messages => IO(messages.foreach(println)))
        .as(ExitCode.Success)

