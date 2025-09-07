package ru.mipt.fp.service

import ru.mipt.fp.cache.Cache
import ru.mipt.fp.domain.{Card, CardCvv, CardExpirationDate, CardNumber, ClientId, Ucid}
import ru.mipt.fp.external.CardsMasterSystemClient

import scala.annotation.nowarn
import scala.concurrent.duration.FiniteDuration

import ru.mipt.fp.masking.Masking

import cats.FlatMap
import cats.syntax.flatMap._
import cats.syntax.functor._

@nowarn
class CardService[F[_]: FlatMap](
  cardsClient: CardsMasterSystemClient[F],
  cardsCache: Cache[F, Ucid, Card],
  cardsListCache: Cache[F, ClientId, List[Card]],
  cacheTtl: FiniteDuration
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
  // тайпкласс Masking нельзя сделать функтором, так как он не удовлетворяет нужным законам:
  // identity law: если мы прогоним карту через маскировку, то получим уже новую отличающуюся строку
  // composition law: не уверен, вроде работать должен, но это странно маскировать несколько раз

  /** Запросить данные всех карт текущего пользователя, маскировать чувствительные данные и вернуть весь список
    *
    * Номер карты из 4444-4444-4444-4567 должен стать 4444-****-****-*567
    *
    * Срок действия и CVV должны замениться на '**\**' и *** соответственно
    *
    * Чтение должно быть кэширующим Из соображений безопасности в кэше не должно быть немаскированных данных по карте
    *
    * Кэширование должно производиться с TTL, указанным в конструкторе
    *
    * При чтении данных из кэша время жизни должно продлеваться
    */
  def getClientCards(clientId: ClientId): F[List[Card]] =
    cardsListCache.get(clientId).flatMap {
      case Some(cards) =>
        cardsListCache.expire(clientId, cacheTtl).map(_ => cards)
      case None =>
        for {
          cards <- cardsClient.getClientCards(clientId)
          maskedCards = cards.map(_.masked)
          _ <- cardsListCache.put(clientId, maskedCards)
          _ <- cardsListCache.expire(clientId, cacheTtl)
        } yield maskedCards
    }

  /** Запросить данные карты из внешнего хранилища по ее Ucid, выполнить маскирование и вернуть информацию
    *
    * Номер карты из 4444-4444-4444-4567 должен стать 4444-****-****-*567
    *
    * Срок действия и CVV должны замениться на **\** и *** соответственно
    *
    * Чтение должно быть кэширующим Из соображений безопасности в кэше не должно быть немаскированных данных по карте
    *
    * Кэширование должно производиться с TTL, указанным в конструкторе
    *
    * При чтении данных из кэша время жизни должно продлеваться
    */
  def getCardById(ucid: Ucid): F[Card] =
    cardsCache.get(ucid).flatMap {
      case Some(card) =>
        cardsCache.expire(ucid, cacheTtl).map(_ => card)
      case None =>
        for {
          card <- cardsClient.getCard(ucid)
          maskedCard = card.masked
          _ <- cardsCache.put(ucid, maskedCard)
          _ <- cardsCache.expire(ucid, cacheTtl)
        } yield maskedCard
    }

  /** Запросить деактивацию карты по ее Ucid
    *
    * Запрос должен инвалидировать кэш для этой карты
    */
  def deactivateCard(ucid: Ucid): F[Unit] =
    for {
      _ <- cardsClient.deactivateCard(ucid)
      _ <- cardsCache.invalidate(ucid)
    } yield ()
