package hw01

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

import scala.util.{Failure, Success, Try}
import scala.util.control.NoStackTrace

class CustomEffectTests extends AnyFlatSpec with Matchers:
    val stringResult = "Hello effectful World!"
    val intResult    = 123456
    val exceptionOne = new Exception("1") with NoStackTrace
    val ecxeptionTwo = new Exception("2") with NoStackTrace
    val executableSucessWithString = Executable.Effect.pure(stringResult)
    val executableSucessWithInt    = Executable.Effect.pure(intResult)
    val executableFailedOne        = Executable.Effect.raise(exceptionOne)
    val executableFailedTwo        = Executable.Effect.raise(ecxeptionTwo)
    val declarativeSuccessOne      = Declarative.Effect.pure(stringResult)
    val declarativeSuccessTwo      = Declarative.Effect.pure(intResult)
    val declarativeFailedOne       = Declarative.Effect.raise(exceptionOne)
    val declarativeFailedTwo       = Declarative.Effect.raise(ecxeptionTwo)


    "both" should "return success with tuple" in {
        executableSucessWithString.both(executableSucessWithInt).safeRun() shouldBe Success(stringResult -> intResult)
    }

    "both" should "return failure if left effect fails" in {
        executableFailedOne.both(executableSucessWithInt).safeRun() shouldBe Failure(exceptionOne)
    }

    "both" should "return failure if right effect fails" in {
        executableSucessWithString.both(executableFailedTwo).safeRun() shouldBe Failure(ecxeptionTwo)
    }

    "both" should "return left failure ifboth effects fails" in {
        executableFailedOne.both(executableFailedTwo).safeRun() shouldBe Failure(exceptionOne)
    }

    "zip" should "return success with tuple" in {
        declarativeSuccessOne.zip(declarativeSuccessTwo).eval shouldBe Declarative.EffectSuccess(stringResult -> intResult)
    }

    "zip" should "return failure if left effect fails" in {
        declarativeFailedOne.zip(declarativeSuccessTwo).eval shouldBe Declarative.EffectFailure(exceptionOne)
    }

    "zip" should "return failure if right effect fails" in {
        declarativeSuccessOne.zip(declarativeFailedTwo).eval shouldBe Declarative.EffectFailure(ecxeptionTwo)
    }

    "zip" should "return left failure if both effects fails" in {
        declarativeFailedOne.zip(declarativeFailedTwo).eval shouldBe Declarative.EffectFailure(exceptionOne)
    }
