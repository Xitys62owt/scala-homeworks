package hw

/**
 * Имплементируйте тесты самостоятельно.
 * Можно тестовые файлы положить в ресурсы, а можно создавать их на лету через `File.createTempFile`
 */
class StudentDatabaseSpec extends munit.FunSuite:

  test("StudentDatabase initialization should read data from init file"):
    assertEquals(1, 0)

  test("StudentDatabase initialization should fail if source file not exists"):
    assertEquals(1, 0)

  test("StudentDatabase initialization should fail if some of link file not exists"):
    assertEquals(1, 0)

  test("StudentDatabase initialization should fail if some of link format is invalid"):
    assertEquals(1, 0)

  test("StudentDatabase should read existing table"):
    assertEquals(1, 0)

  test("StudentDatabase reading table should fail if table not exists"):
    assertEquals(1, 0)

  test("StudentDatabase reading table should fail if row format is invalid"):
    assertEquals(1, 0)

  test("StudentDatabase reading table should fail if grade format is invalid"):
    assertEquals(1, 0)

  test("StudentDatabase reading table should fail if grade format is invalid"):
    assertEquals(1, 0)

  test("StudentDatabase should provide proper information for determining best grade student via StudentStatisics"):
    assertEquals(1, 0)

