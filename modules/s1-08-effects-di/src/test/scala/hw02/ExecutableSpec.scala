package hw02

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}
import scala.util.control.NoStackTrace

class CustomEffectTests extends AnyFlatSpec with Matchers:
    import Executable.*
    val stringResult = "Hello effectful World!"
    val intResult    = 123456
    val exceptionOne = new Exception("1") with NoStackTrace
    val ecxeptionTwo = new Exception("2") with NoStackTrace
    val executableSucessOne = Effect.pure(stringResult)
    val executableSucessTwo = Effect.pure(intResult)
    val executableFailedOne: Effect[String] = Effect.raise(exceptionOne)
    val executableFailedTwo: Effect[Int]    = Effect.raise(ecxeptionTwo)

    "recover" should "result successful effect" in {
        executableFailedOne.recover(_ => stringResult).safeRun() shouldBe Success(stringResult)
    }

    "recoverWith" should "result successful effect in case of recover success" in {
        executableFailedOne.recoverWith(_ => executableSucessOne).safeRun() shouldBe Success(stringResult)
    }

    "recoverWith" should "result failed effect in case of recover failure" in {
        executableFailedOne.recoverWith(_ => executableFailedTwo.map(_ => stringResult)).safeRun() shouldBe Failure(ecxeptionTwo)
    }

