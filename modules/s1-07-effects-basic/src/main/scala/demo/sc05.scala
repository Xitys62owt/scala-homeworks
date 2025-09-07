package sc05

import cats.effect.*

import java.io.{BufferedReader, BufferedWriter, FileReader, FileWriter}

object OutcomeDescribed:
  enum Outcome[F[_], E, A]:
    case Succeeded[F[_], E, A](fa: F[A]) extends Outcome[F, E, A]
    case Errored[F[_], E, A](e: E) extends Outcome[F, E, A]
    case Canceled[F[_], E, A]() extends Outcome[F, E, A]
  type OutcomeIO[A] = Outcome[IO, Throwable, A]

object BracketDescribed:
  import cats.effect.OutcomeIO
  trait IO[A]:
    def bracket[B](use: A => IO[B])(release: A => IO[Unit]): IO[B]
    def bracketCase[B](use: A => IO[B])(release: (A, OutcomeIO[B]) => IO[Unit]): IO[B]
    def guarantee(finalizer: IO[Unit]): IO[A]
    def guaranteeCase(finalizer: OutcomeIO[A] => IO[Unit]): IO[A]

object ResourceExample:
  import cats.effect.Resource
  def bufferedWriter(filename: String): Resource[IO, BufferedWriter] =
    Resource.makeCase(IO(new BufferedWriter(new FileWriter(filename, true)))):
      case (bw, Resource.ExitCase.Succeeded)  => IO(bw.close())
      case (bw, Resource.ExitCase.Errored(_)) => IO(bw.close())
      case (bw, Resource.ExitCase.Canceled)   => IO(bw.close())

  def write(input: String): IO[Unit] =
    bufferedWriter("out.txt").use(bw => IO(bw.write(input)))

  def splitDrains: Resource[IO, (BufferedWriter, BufferedWriter)] =
    for
      drain1 <- bufferedWriter("file1.txt")
      drain2 <- bufferedWriter("file2.txt")
    yield (drain1, drain2)

object CeResourceDemo extends IOApp:

  def bufferedReader(fileName: String): Resource[IO, BufferedReader] =
    Resource.makeCase(IO(new BufferedReader(new FileReader(fileName)))):
      case (br, Resource.ExitCase.Succeeded)   => IO(br.close())
      case (br, Resource.ExitCase.Errored(th)) => IO(println(s"Error: #e")).guarantee(IO(br.close()))
      case (br, Resource.ExitCase.Canceled)    => IO(println("Canceled")).guarantee(IO(br.close()))

  def read(source: BufferedReader) =
    IO(source.readLine)

  def someWork =
    for
      line1 <- bufferedReader("file1.txt").use(read)
      line2 <- bufferedReader("file2.txt").use(read)

      _ <- IO.println(s"$line1\n$line2")
    yield ()

  override def run(args: List[String]) =
    someWork
      .as(ExitCode.Success)
