package ru.mipt.fp.service

import cats.MonadError

import java.time.Instant
import org.scalamock.scalatest.MockFactory
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import ru.mipt.fp.external.{CardsMasterSystemClient, OperationsSystemClient}
import ru.mipt.fp.domain.{Card, NetworkError, Ucid}
import ru.mipt.fp.resilience.{FallbackCache, Retry}
import ru.mipt.fp.utils.{Cache, Timer}
import ru.mipt.fp.testdata.TestData.*

import scala.concurrent.duration.*

class CardsServiceSpec extends AnyFlatSpec with Matchers with MockFactory:

  "getOperationsStatistics" should "return stats for account and cards" in new Wirings:
    val from = Instant.now().minusSeconds(10000)
    val to = Instant.now()

    mockOperationsSystemClient.getAccountOperations expects (accountId, from, to) returns Right(accountOperations)
    mockOperationsSystemClient.getCardOperations expects (ucid, from, to) returns Right(cardOperations)
    mockCardsClient.getCard expects ucid returns Right(card)
    mockCache.put expects (ucid, maskedCard) returns Right(())

    service.getOperationsStatistics(accountId, List(ucid), from, to) shouldBe Right(allOperationStats)

  testRetries(NetworkError.TimeoutError)
  testRetries(NetworkError.NotAvailableError)
  testRetries(NetworkError.InternalError)
  testUnretryableError(NetworkError.NotFoundError)
  testUnretryableError(NetworkError.BadRequestError)

  def testRetries(error: NetworkError): Unit =
    it should s"return empty stats for account and cards if all retries failed and the error is $error" in new Wirings:

      val from = Instant.now().minusSeconds(10000)
      val to = Instant.now()

      (timer.sleep expects delay)
        .returns(Right(()))
        .repeat(6)

      mockOperationsSystemClient.getAccountOperations expects (accountId, from, to) returns Left(error)
      mockOperationsSystemClient.getCardOperations expects (ucid, from, to) returns Left(error)
      mockCardsClient.getCard expects ucid returns Right(card)
      mockCache.put expects (ucid, maskedCard) returns Right(())

      service.getOperationsStatistics(accountId, List(ucid), from, to) shouldBe Right(emptyOperationStats)

    it should s"return stats for account and cards and card from fallback cache if card system is failed with $error" in new Wirings:
      val from = Instant.now().minusSeconds(10000)
      val to = Instant.now()

      (timer.sleep expects delay)
        .returns(Right(()))
        .repeat(3)

      mockOperationsSystemClient.getAccountOperations expects (accountId, from, to) returns Right(accountOperations)
      mockOperationsSystemClient.getCardOperations expects (ucid, from, to) returns Right(cardOperations)
      mockCardsClient.getCard expects ucid returns Left(error)
      mockCache.get expects ucid returns Right(Some(maskedCard))

      service.getOperationsStatistics(accountId, List(ucid), from, to) shouldBe Right(allOperationStats)

    it should s"fail if there is no card fallback cache and card system is failed with $error" in new Wirings:
      val from = Instant.now().minusSeconds(10000)
      val to = Instant.now()

      (timer.sleep expects delay)
        .returns(Right(()))
        .repeat(3)

      mockOperationsSystemClient.getAccountOperations expects (accountId, from, to) returns Right(accountOperations)
      mockOperationsSystemClient.getCardOperations expects (ucid, from, to) returns Right(cardOperations)
      mockCardsClient.getCard expects ucid returns Left(error)
      mockCache.get expects ucid returns Right(None)

      service.getOperationsStatistics(accountId, List(ucid), from, to) shouldBe Left(error)

  def testUnretryableError(error: NetworkError): Unit =
    it should s"return empty stats for account and cards if error is $error" in new Wirings:

      val from = Instant.now().minusSeconds(10000)
      val to = Instant.now()

      mockOperationsSystemClient.getAccountOperations expects (accountId, from, to) returns Left(error)
      mockOperationsSystemClient.getCardOperations expects (ucid, from, to) returns Left(error)
      mockCardsClient.getCard expects ucid returns Right(card)
      mockCache.put expects (ucid, maskedCard) returns Right(())

      service.getOperationsStatistics(accountId, List(ucid), from, to) shouldBe Right(emptyOperationStats)

  trait Wirings:

    type MockF[A] = Either[NetworkError, A]

    val mockCardsClient = mock[CardsMasterSystemClient[MockF]]
    val mockOperationsSystemClient = mock[OperationsSystemClient[MockF]]
    val mockCache = mock[Cache[MockF, Ucid, Card]]

    given timer: Timer[MockF] = mock[Timer[MockF]]
    given FallbackCache[MockF, Ucid, Card] = FallbackCache(mockCache)
    given Retry[MockF, NetworkError] = Retry[MockF, NetworkError]

    val delay = 100.milliseconds

    val service = new CardService[MockF](
      mockOperationsSystemClient,
      mockCardsClient,
      3,
      delay
    )
