package hw03

import zio.*
import zio.test.*
import zio.test.Assertion.*

object ZioEffectSpec extends ZIOSpecDefault:

    override def spec: Spec[TestEnvironment & Scope, Any] =
        suite("Cached read")(
            test("read faster second time") {
                for {
                    cached <- Ref.make(Map.empty[Int, String]).map(FastStorage(SlowStorage, _))
                    f1 <- cached.get(1).timed.fork
                    _ <- TestClock.adjust(1.second)
                    (lat1, author1) <- f1.join
                    f2 <- cached.get(1).timed.fork
                    _ <- TestClock.adjust(1.second)
                    (lat2, author2) <- f2.join
                } yield
                    assert(author1 == author2)(isTrue) &&
                    assert(10L)(isLessThanEqualTo(lat1.toMillis / lat2.toMillis))
            }
        )
