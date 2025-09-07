package hw

import java.nio.file.{Files, Path, Paths}
import scala.annotation.nowarn
import scala.io.Source
import scala.util.{Try, Using}

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

sealed trait GetTableError
case object TableNotFound extends GetTableError
case class FileReadError(message: String) extends GetTableError
case class InvalidRowFormat(row: String) extends GetTableError
case class InvalidGradeFormat(row: String) extends GetTableError

sealed trait DbInitError
case class FileError(message: String) extends DbInitError
case class SourceFileNotFound(name: String) extends DbInitError
case class InvalidTableLinkFormat(line: String) extends DbInitError
case class TableFileNotFound(path: String) extends DbInitError

// расширил ошибки "по мотивам тестов" (плюс если просто type не компилится)

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
    def getTable(tableName: String): Either[GetTableError, Vector[Student]] =
      tables.get(tableName) match
        case Some(path) =>
          if (!Files.exists(path))
            return Left(TableNotFound)
          Using(Source.fromFile(path.toFile)) { source =>
            source.getLines().toVector.flatMap { line =>
              line.split(",") match
                case Array(name, gradeStr) =>
                  Try(gradeStr.toShort).toOption match
                    case Some(grade) => Some(Right(Student(name, grade)))
                    case None        => Some(Left(InvalidGradeFormat(line)))
                case _ => Some(Left(InvalidRowFormat(line)))
            }
          }.toEither.left
            .map(ex => FileReadError(ex.getMessage))
            .flatMap(results =>
              if (results.exists(_.isLeft))
                Left(results.collect { case Left(err) => err }.head)
              else
                Right(results.collect { case Right(student) => student })
            )
        case _ => Left(TableNotFound)
  }

  /**
   * Загружает базу данных из файла sourceFileName.
   */
  def readFromFile(sourceFileName: String): Either[DbInitError, StudentDatabase] =
    val path = Paths.get(sourceFileName)
    if (!Files.exists(path))
      return Left(SourceFileNotFound(sourceFileName))
    val lines =
      Using(Source.fromFile(path.toFile))(_.getLines().toVector).toEither.left.map(ex => FileError(ex.getMessage))
    lines.flatMap { lines =>
      val tables = lines.map { line =>
        line.split('|') match
          case Array(name, path) =>
            val filePath = Paths.get(path)
            if (Files.exists(filePath))
              Right(name.trim -> filePath)
            else
              Left(TableFileNotFound(path))
          case _ => Left(InvalidTableLinkFormat(line))
      }
      if (tables.exists(_.isLeft))
        Left(tables.collect { case Left(err) => err }.head)
      else
        Right(new Impl(tables.collect { case Right(entry) => entry }.toMap))
    }

}

object StudentStatistics: // забавную орфографическую ошибку убрал
  /**
   * Возвращает студента с максимальным баллом из конкретной таблицы.
   */
  def getBestGradeStudent(
    db: StudentDatabase,
    tableName: String
  ): Either[GetTableError, Option[Student]] = db.getTable(tableName).map(_.maxByOption(_.grade))
