package sc03

import zio.*

object ZioRacingDemo extends ZIOAppDefault:

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
    def get(idx: Int): UIO[Option[String]] =
      ZIO.sleep(100.millis) *> ZIO.succeed(bigStorage.get(idx))
    def apply(idx: Int): Task[String] =
      ZIO.sleep(100.millis) *> ZIO.attempt(bigStorage(idx))

  object FastStorage:
    private val smallStorage = Map(
      3 -> "Martin Odersky",
      4 -> "Robert Martin",
      7 -> "Edgar Frank Codd",
      9 -> "Alonzo Church",
      10 -> "Kurt Friedrich Gödel",
      11 -> "David Hilbert"
    )
    def get(idx: Int): UIO[Option[String]] =
      ZIO.sleep(10.millis) *> ZIO.succeed(smallStorage.get(idx))
    def apply(idx: Int): Task[String] =
      ZIO.sleep(10.millis) *> ZIO.attempt(smallStorage(idx))

  // -------------------------------------------------------------

  def logLatency[E, A](zio: ZIO[Any, E, A]): ZIO[Any, E, A] =
    for
      (lat, res) <- zio.timed
      _ <- ZIO.logInfo(s"Give $res. Take ${lat.toMillis()}ms")
    yield res

  override def run =
    fetchCached.exitCode

  val fetchAll =
    ZIO
      .foreachPar(1 to 11): idx =>
        logLatency(FastStorage.get(idx).zipPar(SlowStorage.get(idx))).map(_ orElse _)
      .withParallelism(16)
      .map(_.toList)

  val fetchFast =
    ZIO
      .foreachPar(1 to 11): idx =>
        logLatency(FastStorage.get(idx) race SlowStorage.get(idx))
      .withParallelism(16)
      .map(_.toList)

  val fetchCached =
    ZIO
      .foreachPar(1 to 11): idx =>
        logLatency(FastStorage(idx).orElse(SlowStorage(idx)))
      .withParallelism(16)
      .map(_.toList)
