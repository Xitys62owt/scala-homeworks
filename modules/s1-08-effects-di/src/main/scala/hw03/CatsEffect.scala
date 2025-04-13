package hw03

import cats.effect.*
import cats.effect.kernel.Ref
import cats.effect.std.Random
import cats.syntax.all.*

import scala.annotation.nowarn
import scala.concurrent.duration.*
import scala.util.control.NoStackTrace

case class Author(idx: Int, name: String)

case object TimeoutException extends Exception("Request timeout") with NoStackTrace

trait AuthorService:
  def get(idx: Int): IO[Author]

class SlowStorage(random: Random[IO]) extends AuthorService:
  private val storage = Map(
    1 -> "John Doe",
    2 -> "Joan Doe",
    3 -> "Martin Odersky",
    4 -> "Robert Martin",
    5 -> "Isaak Asimov",
    6 -> "Stanislav Lem",
    7 -> "Edgar Frank Codd",
    8 -> "Alan Mathison Turing",
    9 -> "Alonzo Church",
    10 -> "Kurt Friedrich Gödel",
    11 -> "David Hilbert"
  )

  override def get(idx: Int): IO[Author] =
    for {
      latency <- random.betweenInt(100, 1000)
      _ <- IO.sleep(latency.millis)
      author <- IO(Author(idx, storage(idx)))
    } yield author

@nowarn
class FastStorage(delegate: AuthorService, cache: Ref[IO, Map[Int, String]]) extends AuthorService:

  def limitedRead(io: IO[Author], timeout: FiniteDuration): IO[Author] =
    IO.race(io, IO.raiseError[Author](TimeoutException).delayBy(timeout)).map(_.merge)

  /**
      * Допишите метод чтения записи из кэша, если она там есть.
      * Если запись есть, надо вернуть Some[Author].
      * Если записи нет, надо вернуть None
      * 
      * Метод должен работать с задержкой 10ms, которая уже присутствует в коде
      *
      * @param idx индекс автора
      * @return IO-эффект с записью об авторе или отсутствием значения
      */
  def getCached(idx: Int): IO[Option[Author]] =
    IO.sleep(10.millis) *>
      ???

  /**
      * Допишите метод для размщения записи в кэше.
      * Метод должен обновлять кэш, добавляя в него одну запись
      * * Дополнительно: Метод должен добавлять в кэш запись на 1 секунду
      *
      * Метод должен работать с задержкой 10ms, которая уже присутствует в коде
      * 
      * @param author запись об авторе, которая должна быть помещена в кэш
      * @return IO-монаду с пустым значением
      */
  def putCached(author: Author): IO[Unit] =
    IO.sleep(10.millis) *>
      ???

  /**
      * Допишите метод fallback-кешированного чтения. Первую строку оставьте нетронутой. Мы не должны ждать результата дольше чем 0.5с
      * 
      * Работать метод должен следующим образом:
      *  *  Если бэкенд вернул запись, то пишет ее в кэш и возвращает
      *  *  Если бэкенд выбросил исключение:
      *  *  * Читает из кэша
      *  *  *  * Если кэш вернул запись, то возвращает эту запись
      *  *  *  * Если кэш не вернул запись, выбрасывает исходную ошибку
      *
      * @param idx индекс искомого автора
      * @return IO-эффект с записью об авторе
      */
  override def get(idx: Int): IO[Author] =
    limitedRead(delegate.get(idx), 500.millis)
