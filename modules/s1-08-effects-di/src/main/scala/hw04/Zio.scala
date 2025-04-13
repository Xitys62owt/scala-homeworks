package hw04

import zio.*
import zio.Duration.*

import scala.annotation.nowarn
import scala.util.control.NoStackTrace

case class Author(idx: Int, name: String)

case object TimeoutException extends Exception("Request timeout") with NoStackTrace

trait AuthorService:
  def get(idx: Int): Task[Author]

object SlowStorage extends AuthorService:
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

  override def get(idx: Int): Task[Author] =
    for {
      latency <- Random.nextIntBetween(100, 1000)
      _ <- ZIO.sleep(latency.millis)
      author <- ZIO.attempt(Author(idx, storage(idx)))
    } yield author

@nowarn
class FastStorage(delegate: AuthorService, cache: Ref[Map[Int, String]]) extends AuthorService:

  def limitedRead(io: Task[Author], timeout: Duration): Task[Author] =
    io.raceFirst((ZIO.logError("Timeout") *> ZIO.fail(TimeoutException)).delay(timeout))

  /**
      * Напишите метод чтения записи из кэша, если она там есть.
      * Если запись есть, надо вернуть Some[Author].
      * Если записи нет, надо вернуть None
      * 
      * Метод должен работать с задержкой 10ms, которая уже присутствует в коде
      *
      * @param idx индекс автора
      * @return IO-эффект с записью об авторе или отсутствием значения
      */
  def getCached(idx: Int): UIO[Option[Author]] =
    ZIO.sleep(10.millis) *>
      ???

  /**
      * Напишите метод для размщения записи в кэше.
      * Метод должен обновлять кэш, добавляя в него одну запись
      *
      * Метод должен работать с задержкой 10ms, которая уже присутствует в коде
      * 
      * @param author запись об авторе, которая должна быть помещена в кэш
      * @return IO-монаду с пустым значением
      */
  def putCached(author: Author): UIO[Unit] =
    ZIO.sleep(10.millis) *>
      ???

  /**
      * Допишите метод fallback-кешированного чтения. Первую строку оставьте нетронутой. Мы не должны ждать результата дольше чем 0.5с
      * 
      * Работать метод должен следующим образом:
      *  *  Если бэкенд вернул запись, то пишет ее в кэш и возвращает
      *  *  Если бэкенд выбросил исключение:
      *  *  * Читает кэша
      *  *  *  * Если кэш вернул запись, то возвращает эту запись
      *  *  *  * Если кэш не вернул запись, выбрасывает исходную ошибку
      *
      * @param idx индекс искомого автора
      * @return IO-эффект с записью об авторе
      */
  override def get(idx: Int): Task[Author] =
    limitedRead(delegate.get(idx), 500.millis)
