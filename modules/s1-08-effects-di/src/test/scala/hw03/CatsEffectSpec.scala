package hw03

import cats.effect.*
import cats.effect.kernel.Ref
import cats.effect.std.Random
import cats.effect.testing.scalatest.AsyncIOSpec
//import cats.syntax.all.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.duration.*

class CatsEffectTests extends AsyncFlatSpec with AsyncIOSpec with Matchers:

  def newTestRandom(randoms: Vector[Int]): IO[Random[IO]] =
    Ref[IO]
      .of(randoms)
      .map: values =>
        new Random[IO] {

          override def betweenInt(minInclusive: Int, maxExclusive: Int): IO[Int] =
            values.getAndUpdate(source => source.drop(1) appended source(0)).map(_(0))

          override def betweenDouble(minInclusive: Double, maxExclusive: Double): IO[Double] = ???
          override def betweenFloat(minInclusive: Float, maxExclusive: Float): IO[Float] = ???
          override def betweenLong(minInclusive: Long, maxExclusive: Long): IO[Long] = ???
          override def nextAlphaNumeric: IO[Char] = ???
          override def nextBoolean: IO[Boolean] = ???
          override def nextBytes(n: Int): IO[Array[Byte]] = ???
          override def nextDouble: IO[Double] = ???
          override def nextFloat: IO[Float] = ???
          override def nextGaussian: IO[Double] = ???
          override def nextInt: IO[Int] = ???
          override def nextIntBounded(n: Int): IO[Int] = ???
          override def nextLong: IO[Long] = ???
          override def nextLongBounded(n: Long): IO[Long] = ???
          override def nextPrintableChar: IO[Char] = ???
          override def nextString(length: Int): IO[String] = ???
          override def shuffleList[A](l: List[A]): IO[List[A]] = ???
          override def shuffleVector[A](v: Vector[A]): IO[Vector[A]] = ???
        }

  "repetable successful read" should "read from external service only" in:
    for
      ref <- Ref[IO].of(Map.empty[Int, String])
      random <- newTestRandom(Vector(100, 100))
      cached = FastStorage(SlowStorage(random), ref)
      (lat1, author1) <- cached.get(1).timed
      (lat2, author2) <- cached.get(1).timed
      _ = Math.abs(lat1.toMillis - lat2.toMillis) should be <= 100L
      _ = author1 shouldBe author2
    yield ()

  "failed repeated read" should "read from cache" in:
    for
      ref <- Ref[IO].of(Map.empty[Int, String])
      random <- newTestRandom(Vector(100, 999))
      cached = FastStorage(SlowStorage(random), ref)
      (lat1, author1) <- cached.get(1).timed
      (lat2, author2) <- cached.get(1).timed
      _ = Math.abs(lat1.toMillis - lat2.toMillis) should be <= 1000L
      _ = author1 shouldBe author2
    yield ()

  "double failed read" should "return the TimeoutException" in:
    for
      ref <- Ref[IO].of(Map.empty[Int, String])
      random <- newTestRandom(Vector(999, 999))
      cached = FastStorage(SlowStorage(random), ref)
      (lat1, author1) <- cached.get(1).map(Right(_)).handleError(Left(_)).timed
      (lat2, author2) <- cached.get(1).map(Right(_)).handleError(Left(_)).timed
      _ = Math.abs(lat1.toMillis - lat2.toMillis) should be <= 100L
      _ = author1 shouldBe author2
      _ = author1 shouldBe Left(TimeoutException)
    yield ()
