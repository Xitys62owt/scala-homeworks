package sc02

import cats.Traverse
import cats.effect.*
import cats.effect.implicits.* 
import cats.effect.implicits.given 
import cats.syntax.parallel.*
import scala.concurrent.duration.{Duration, DurationInt}


object CeRacingDemo extends IOApp:


  object SlowStorage:
    private val bigStorage = Map(
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
    def get(idx: Int): IO[Option[String]] =
      IO.sleep(100.millis) *> IO(bigStorage.get(idx))
    def apply(idx: Int): IO[String] =
      IO.sleep(100.millis) *> IO(bigStorage(idx))


  object FastStorage:
    private val smallStorage = Map(
      3 -> "Martin Odersky",
      4 -> "Robert Martin",
      7 -> "Edgar Frank Codd",
      9 -> "Alonzo Church",
      10 -> "Kurt Friedrich Gödel",
      11 -> "David Hilbert"
    )
    def get(idx: Int): IO[Option[String]] =
      IO.sleep(10.millis) *> IO(smallStorage.get(idx))
    def apply(idx: Int): IO[String] =
      IO.sleep(10.millis) *> IO(smallStorage(idx))

  // -------------------------------------------------------------

  def logLatency[A](io: IO[A]): IO[A] =
    for 
      (lat, res) <- io.timed
      _ <- IO.println(s"Give $res. Take ${lat.toMillis}ms")
    yield res



  override def run(args: List[String]) =
    fetchCached
      .as(ExitCode.Success)

  


  val fetchAll =
    IO.parTraverseN(16)((1 to 11).toList):
      idx => logLatency(FastStorage.get(idx).both(SlowStorage.get(idx))).map(_ orElse _)




  val fetchFast =
    IO.parTraverseN(16)((1 to 11).toList):
      idx => logLatency(FastStorage.get(idx) race SlowStorage.get(idx))
    .map(_.toList)


    

  val fetchCached =
    IO.parTraverseN(16)((1 to 11).toList):
      idx => logLatency(FastStorage(idx).orElse(SlowStorage(idx)))
    .map(_.toList)



