package hw03

import zio.*
import zio.Duration.*

import scala.annotation.nowarn

case class Author(idx: Int, name: String)

trait AuthorService:
  def get(idx: Int): UIO[Option[Author]]

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

  override def get(idx: Int): UIO[Option[Author]] =
    ZIO.sleep(100.millis) *> ZIO.succeed(storage.get(idx).map(Author(idx, _)))

@nowarn
class FastStorage(delegate: AuthorService, cache: Ref[Map[Int, String]]) extends AuthorService:

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
      * Напишите метод кэширующего чтения
      * 
      * Работать он должен следующим образом:
      *  * Читает запись из кэша
      *  *  * Если в кэше нашлать запись, то возвращает ее
      *  *  * Если к кэше нет записи:
      *  *  *  * Читает запись из бекенда(delegate)
      *  *  *  *  * Если бэкенд вернул запись, пишет ее в кэш
      *  *  *  *  * Если бээкенд не вернул запись, ничего не делает
      *  *  * Возвращает ответ бекэнда
      *
      * @param idx индекс искомого автора
      * @return IO-эффект с записью об авторе или отсутствием значения
      */
  override def get(idx: Int): UIO[Option[Author]] =
    ???
