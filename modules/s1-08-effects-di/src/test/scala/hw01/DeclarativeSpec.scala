package hw01

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

//import scala.util.{Failure, Success, Try}
import scala.util.control.NoStackTrace

class CustomEffectTests extends AnyFlatSpec with Matchers:
  import Declarative.*
  val stringResult = "Hello effectful World!"
  val intResult = 123456
  val exceptionOne = new Exception("1") with NoStackTrace
  val ecxeptionTwo = new Exception("2") with NoStackTrace
  val declarativeSuccessOne = Effect.pure(stringResult)
  val declarativeSuccessTwo = Effect.pure(intResult)
  val declarativeFailedOne: Effect[Exception, String] = Effect.raise(exceptionOne)
  val declarativeFailedTwo: Effect[Exception, Int] = Effect.raise(ecxeptionTwo)

  "recover" should "result successful effect" in {
    declarativeFailedOne.recover(_ => stringResult).eval shouldBe EffectSuccess(stringResult)
  }

  "recoverWith" should "result successful effect in case of recover success" in {
    declarativeFailedOne.recoverWith(_ => declarativeSuccessOne).eval shouldBe EffectSuccess(stringResult)
  }

  "recoverWith" should "result failed effect in case of recover failure" in {
    declarativeFailedOne.recoverWith(_ => declarativeFailedTwo.map(_ => stringResult)).eval shouldBe EffectFailure(
      ecxeptionTwo
    )
  }
