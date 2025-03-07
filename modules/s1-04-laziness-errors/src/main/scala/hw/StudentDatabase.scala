package hw

import java.nio.file.Path
import scala.annotation.nowarn

/** 1. База данных студентов оценками.
 *
 * Необходимо написать имплементацию аналога базы данных студентов.
 * База студентов состоит из разных таблиц студентов (их имен).
 * База должны вычитываться из загрузочного файла, в котором хранятся ссылки на таблицы.
 * По сути файл состоит из набора строк вида "<название_таблицы>|<путь до файла таблицы>".
 *
 * При первоначальной загрузке базы, необходимо парсить ссылки на таблицы и проверять, существует ли файл этой таблицы.
 * По итогу инициализации, если хотя бы одна ссылка не была распарсена или один файл таблицы из распарсенной ссылки не был найден,
 * то должна формироваться ошибка, в которой содержится информация обо всех таких подошибках.
 * При обращении к конкретному списку студентов через интерфейс StudentDatabase файл должен вычитываться заново.
 * Если таблицы с конректным именем не существует или произошла системная ошибка чтения файла таблицы, то должна формироваться ошибка.
 *
 * Смоделируйте ошибки, имплементируйте нереализованные методы и напишите тесты.
 */

type GetTableError

type DbInitError

case class Student(name: String, grade: Short)

trait StudentDatabase {

  /**
   * Возвращает содержимое таблицы студентов по имени таблицы.
   */
  def getTable(tableName: String): Either[GetTableError, Vector[Student]]
}

object StudentDatabase {
  @nowarn
  private final class Impl(tables: Map[String, Path]) extends StudentDatabase {
    def getTable(tableName: String): Either[GetTableError, Vector[Student]] = ???
  }

  /**
   * Загружает базу данных из файла sourceFileName.
   */
  def readFromFile(
    sourceFileName: String
  ): Either[DbInitError, StudentDatabase] = ???

}

object StudentStatisics:
  /**
   * Возвращает студента с максимальным баллом из конкретной таблицы.
   */
  def getBestGradeStudent(
    db: StudentDatabase,
    tableName: String
  ): Either[GetTableError, Option[Student]] = ???
