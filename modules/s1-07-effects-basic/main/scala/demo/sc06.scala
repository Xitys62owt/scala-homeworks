package sc06

import zio.*
import zio.Console.*
import zio.Duration.*
import java.io.*




object ExternalSystem:

  def openFile(fileName: String): Task[BufferedReader] =
    ZIO.attemptBlocking(new BufferedReader(new FileReader(fileName)))

  def closeFile(fileReader: BufferedReader): UIO[Unit] =
    ZIO.attemptBlocking(fileReader.close()).ignore

  def readFile(fileReader: BufferedReader): Task[String] =
    ZIO.attemptBlocking(fileReader.readLine())



// >>> AcquireRelease Demo <<< //

object AcquireReleaseDemo extends ZIOAppDefault:
  import ExternalSystem.*

  val reading =
    ZIO.acquireReleaseWith(openFile("file1.txt"))(closeFile(_)): file1 =>
      ZIO.acquireReleaseWith(openFile("file2.txt"))(closeFile(_)): file2 =>
        readFile(file1) zip readFile(file2)

  val run =
    for
      (text1, text2) <- reading
      _ <- zio.Console.printLine(text1)
      _ <- zio.Console.printLine("")
      _ <- zio.Console.printLine(text2)
    yield ()





// >>> Scope Demo <<< //


object ScopeDemo extends ZIOAppDefault:
  import ExternalSystem.*

  def openFileSafe(fileName: String): ZIO[Scope, Throwable, BufferedReader] =
    for
      file <- openFile(fileName)
      _    <- ZIO.addFinalizer(closeFile(file) *> printLine(s"Closed: $fileName").ignore)
    yield file

  def autoClosableFile(fileName: String): ZIO[Scope, Throwable, BufferedReader] =
    ZIO.fromAutoCloseable(openFile("file1.txt")) <*
      ZIO.addFinalizer(printLine(s"Closed: $fileName").ignore)


  val run: ZIO[Scope, Any, Any] =
    ZIO.addFinalizer(printLine("Pre").ignore) *>
    /*ZIO.scoped*/ {
      for
        file1 <- openFileSafe("file1.txt")
        _     <- ZIO.addFinalizer(printLine("Some time before next").ignore.delay(3.second))
        file2 <- autoClosableFile("file2.txt")

        text1 <- readFile(file1)
        text2 <- readFile(file2)

        _ <- printLine(text1)
        _ <- printLine("")
        _ <- printLine(text2)
      yield ()
    } *> 
    ZIO.addFinalizer(printLine("Post").ignore)
