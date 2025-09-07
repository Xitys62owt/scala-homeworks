package sc06

import zio.*
import zio.Console.*

object ZioRefDescribed:

  abstract class Ref[A]:
    def get: UIO[A]
    def set(a: A): UIO[Unit]
    def modify[B](f: A => (B, A)): UIO[B]
    def update(f: A => A): UIO[Unit]
    def setAsync(a: A)(implicit unsafe: Unsafe): Unit
    def updateSome(pf: PartialFunction[A, A]): Unit
    // и другие методы

object ZioRefDemo extends ZIOAppDefault:

  def registerMessage(message: String, ref: Ref[List[String]]): UIO[Unit] =
    ref.update((list: List[String]) => message :: list)

  def readIncomingQueue(latency: Duration, message: String) =
    ZIO.succeed(message).delay(latency)

  val register = for
    ref <- Ref.make(List.empty[String])
    _ <- ZIO.foreachPar((1 to 10).toList): idx =>
      readIncomingQueue(((17 % idx) * 100).millis, s"Recieved $idx message")
        .flatMap(msg => registerMessage(msg, ref))
    reg <- ref.get
  yield reg

  val run =
    register
      .flatMap(messages => ZIO.foreach(messages)(msg => printLine(msg)))
