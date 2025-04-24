package ru.mipt.fp.service

import java.time.Instant

import cats.{Applicative, ApplicativeError}
import cats.syntax.all.*

import ru.mipt.fp.domain.{AccountId, Card, CardNumber, CardOperationStats, NetworkError, Operation, OperationStats, Ucid}
import ru.mipt.fp.domain.NetworkError._
import ru.mipt.fp.external.{CardsMasterSystemClient, OperationsSystemClient}
import ru.mipt.fp.resilience.{Retry, FallbackCache}
import ru.mipt.fp.resilience.Retry.*
import ru.mipt.fp.resilience.FallbackCache.*

import scala.concurrent.duration.FiniteDuration

class CardService[F[_]: Applicative](
  operationsSystemClient: OperationsSystemClient[F],
  cardsMasterSystemClient: CardsMasterSystemClient[F],
  retryCount: Int,
  retryDelay: FiniteDuration
)(using ApplicativeError[F, NetworkError], Retry[F, NetworkError], FallbackCache[F, Ucid, Card]):

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
  ): F[OperationStats] = ???
