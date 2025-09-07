package ru.mipt.fp.service

import java.time.Instant
import cats.{Applicative, ApplicativeError}
import ru.mipt.fp.domain.{AccountId, Card, NetworkError, OperationStats, Ucid}
import ru.mipt.fp.external.{CardsMasterSystemClient, OperationsSystemClient}
import ru.mipt.fp.resilience.{FallbackCache, Retry}

import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration

import ru.mipt.fp.masking.Masking
import cats.syntax.all._
import cats.syntax.traverse._
import ru.mipt.fp.domain.{CardOperationStats, Operation, CardNumber, CardCvv, CardExpirationDate}

@nowarn
class CardService[F[_]: Applicative](
  operationsSystemClient: OperationsSystemClient[F],
  cardsMasterSystemClient: CardsMasterSystemClient[F],
  retryCount: Int,
  retryDelay: FiniteDuration
)(using
  error: ApplicativeError[F, NetworkError],
  retry: Retry[F, NetworkError],
  fallbackCache: FallbackCache[F, Ucid, Card]
):

  given Masking[Card] with
    def mask(card: Card): Card =
      val parts = card.number.cardNumber.split("-")
      val maskedNumber = s"${parts(0)}-****-****-*${parts(3).takeRight(3)}"
      card.copy(
        number = CardNumber(maskedNumber),
        cvv = CardCvv("***"),
        expirationDate = CardExpirationDate("**\\**")
      )

  private def retryFlag(error: NetworkError): Boolean =
    error match
      case NetworkError.TimeoutError | NetworkError.NotAvailableError | NetworkError.InternalError => true
      case _                                                                                       => false

  private def getCardWithRetry(ucid: Ucid): F[Card] =
    val operation = retry
      .retry(cardsMasterSystemClient.getCard(ucid))(retryFlag)(retryCount, retryDelay)
      .map(_.masked)
    fallbackCache.withFallback(ucid) { _ =>
      operation
    }

  private def getOperationsWithRetry(getOperations: => F[List[Operation]]): F[List[Operation]] =
    retry
      .retry(getOperations)(retryFlag)(retryCount, retryDelay)
      .handleError(_ => List.empty[Operation])

  /** Запросить статистику по операциям по счету и выбранным картам за указанный период
    *
    * Статистику сгруппировать по доходным и расходным операциям
    *
    * Номер карты должен быть маскирован следующим образом: 1234-****-****-*789
    *
    * В кэше данные карт должны быть маскированы: номер карты как выше, вместо CVV "***" и вместо даты "**\**"
    *
    * Сетевые ошибки 5xx (Timeout, NotAvailable и InternalError) должны ретраиться, а ошибки 4хх (BadRequest и NotFound) - нет. Это нужно сделать с помощью Retry. 
    *
    * При исчерпании попыток должен возвращаться пустой список (Null Object pattern) для списков, и использоваться fallback cache для карт
    */
  def getOperationsStatistics(
    accountId: AccountId,
    cardIds: List[Ucid],
    from: Instant,
    to: Instant
  ): F[OperationStats] =
    val accountOperations = getOperationsWithRetry(
      operationsSystemClient.getAccountOperations(accountId, from, to)
    )
    val cardOperations = cardIds.traverse { ucid =>
      (getCardWithRetry(ucid), getOperationsWithRetry(operationsSystemClient.getCardOperations(ucid, from, to)))
        .mapN { case (card, operations) =>
          val (income, outcome) = operations.foldLeft((0.0, 0.0)) { case ((inc, out), Operation(_, amount)) =>
            if (amount > 0)
              (inc + amount, out)
            else
              (inc, out - amount)
          }
          CardOperationStats(card.number, income, outcome)
        }
    }
    (accountOperations, cardOperations).mapN { (accOps, cardOps) =>
      val (accIncome, accOutcome) = accOps.foldLeft((0.0, 0.0)) { case ((inc, out), Operation(_, amount)) =>
        if (amount >= 0) (inc + amount, out) else (inc, out + amount)
      }
      OperationStats(
        income = accIncome,
        outcome = accOutcome,
        cardOperations = cardOps
      )
    }
