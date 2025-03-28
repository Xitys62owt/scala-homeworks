package hw02

import cats.effect.*
import cats.effect.kernel.Ref
import cats.effect.testing.scalatest.AsyncIOSpec
import cats.syntax.all.*
import org.scalatest.flatspec.AsyncFlatSpec
import org.scalatest.matchers.should.Matchers
import scala.concurrent.duration.*


class CatsEffectTests extends AsyncFlatSpec with AsyncIOSpec with Matchers:

    "cached read" should "read faster second time" in:
        for
            cached <- Ref[IO].of(Map.empty[Int, String]).map(FastStorage(SlowStorage, _))
            (lat1, author1) <- cached.get(1).timed
            (lat2, author2) <- cached.get(1).timed
            _ = lat1.toMillis/lat2.toMillis should be >= 10L
            _ = author1 shouldBe author2
        yield ()
