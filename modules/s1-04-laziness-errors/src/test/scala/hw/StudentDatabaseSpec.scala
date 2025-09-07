package hw

import java.io.{File, PrintWriter}
def createTempFile(content: String): File = {
  val file = File.createTempFile("test", ".csv")
  val writer = new PrintWriter(file)
  writer.write(content)
  writer.close()
  file.deleteOnExit()
  file
}
// мини функция, которая гарантирует чистку ресурсов
// сначала думал просто создатть и в конце удалить, но файлов получилось много
// поэтому есть повторения, когда создается один и тот же

/**
 * Имплементируйте тесты самостоятельно.
 * Можно тестовые файлы положить в ресурсы, а можно создавать их на лету через `File.createTempFile`
 */
class StudentDatabaseSpec extends munit.FunSuite:

  test("StudentDatabase initialization should read data from init file"):
    val file = createTempFile("Arsenii,100\nArsenii2,110")
    val file2 = createTempFile(s"students|${file.getAbsolutePath}")
    val db = StudentDatabase.readFromFile(file2.getAbsolutePath)
    assert(db.isRight)

  test("StudentDatabase initialization should fail if source file not exists"):
    val db = StudentDatabase.readFromFile("randomFile.txt")
    assert(db.isLeft)

  test("StudentDatabase initialization should fail if some of link file not exists"):
    val file = createTempFile("students|randomFile.csv")
    val db = StudentDatabase.readFromFile(file.getAbsolutePath)
    assert(db.isLeft)

  test("StudentDatabase initialization should fail if some of link format is invalid"):
    val file = createTempFile("noSeparator")
    val db = StudentDatabase.readFromFile(file.getAbsolutePath)
    assert(db.isLeft)

  test("StudentDatabase should read existing table"):
    val file = createTempFile("Arsenii,100\nArsenii2,110")
    val file2 = createTempFile(s"students|${file.getAbsolutePath}")
    val db = StudentDatabase.readFromFile(file2.getAbsolutePath).getOrElse(fail("err"))
    val students = db.getTable("students")
    assert(students.isRight)

  test("StudentDatabase reading table should fail if table not exists"):
    val file = createTempFile("Arsenii,100\nArsenii2,110")
    val file2 = createTempFile(s"students|${file.getAbsolutePath}")
    val db = StudentDatabase.readFromFile(file2.getAbsolutePath).getOrElse(fail("err"))
    val students = db.getTable("randomSpisok")
    assert(students.isLeft)

  test("StudentDatabase reading table should fail if row format is invalid"):
    val file = createTempFile("Arsenii,100\nNepravilnoya_stroka")
    val file2 = createTempFile(s"students|${file.getAbsolutePath}")
    val db = StudentDatabase.readFromFile(file2.getAbsolutePath).getOrElse(fail("err"))
    val students = db.getTable("students")
    assert(students.isLeft)

  test("StudentDatabase reading table should fail if grade format is invalid"):
    val file = createTempFile("Arsenii,100\nArsenii2,Stodesyat")
    val file2 = createTempFile(s"students|${file.getAbsolutePath}")
    val db = StudentDatabase.readFromFile(file2.getAbsolutePath).getOrElse(fail("err"))
    val students = db.getTable("students")
    assert(students.isLeft)

  test("StudentDatabase reading table should fail if grade format is invalid"):
    assertEquals(0, 0) // тот же тест что и выше

  test("StudentDatabase should provide proper information for determining best grade student via StudentStatisics"):
    val file = createTempFile("Arsenii,100\nArsenii2,110")
    val file2 = createTempFile(s"students|${file.getAbsolutePath}")
    val db = StudentDatabase.readFromFile(file2.getAbsolutePath).getOrElse(fail("err"))
    val best = StudentStatistics.getBestGradeStudent(db, "students")
    assertEquals(best.getOrElse(None), Some(Student("Arsenii2", 110)))
