package hw04

import zio.*
import zio.test.*
import zio.test.Assertion.*
import zio.test.TestAspect.sequential

object ZioEffectSpec extends ZIOSpecDefault:

    override def spec: Spec[TestEnvironment & Scope, Any] =
        suite("Cached read")(
            test("repetable successful read") {
                for {
                    _       <- TestRandom.feedInts(100, 100)
                    cached  <- Ref.make(Map.empty[Int, String]).map(FastStorage(SlowStorage, _))
                    f1      <- cached.get(1).fork
                    _       <- TestClock.adjust(200.millis)
                    author1 <- f1.join
                    f2      <- cached.get(1).fork
                    _       <- TestClock.adjust(200.millis)
                    author2 <- f2.join
                } yield
                    assert(author1 == author2)(isTrue)
            },
            test("successful read from fallback cache") {
                for {
                    _       <- TestRandom.feedInts(100, 999)
                    cached  <- Ref.make(Map.empty[Int, String]).map(FastStorage(SlowStorage, _))
                    f1      <- cached.get(1).fork
                    _       <- TestClock.adjust(200.millis)
                    author1 <- f1.join
                    f2      <- cached.get(1).fork
                    _       <- TestClock.adjust(1200.millis)
                    author2 <- f2.join
                } yield
                    assert(author1 == author2)(isTrue)
            },
            test("fail read from source and fallback cache") {
                for {
                    _       <- TestRandom.feedInts(999, 999)
                    cached  <- Ref.make(Map.empty[Int, String]).map(FastStorage(SlowStorage, _))
                    f1      <- cached.get(1).either.fork
                    _       <- TestClock.adjust(1200.millis)
                    author1 <- f1.join
                    f2      <- cached.get(1).either.fork
                    _       <- TestClock.adjust(1200.millis)
                    author2 <- f2.join
                } yield
                    assert(author1 == author2)(isTrue) &&
                    assertTrue(author1 == Left(TimeoutException))
            }

        ) @@ sequential
