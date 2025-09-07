package ru.mipt.fp.testdata

import ru.mipt.fp.domain.{AccountId, Card, CardCvv, CardExpirationDate, CardNumber, CardOperationStats, ClientId, Operation, OperationStats, Ucid}

object TestData {

  val clientId = ClientId("1234567890")
  val accountId = AccountId("1234567890")
  val accountOperations = List(
    Operation(clientId, 200),
    Operation(clientId, -100),
    Operation(clientId, -1200),
    Operation(clientId, 1100)
  )

  val ucid = Ucid("Ucid")
  val cardNumber = CardNumber("1234-5678-9012-3456")
  val cvv = CardCvv("123")
  val expirationDate = CardExpirationDate("01/20")
  val card = Card(ucid, cardNumber, cvv, expirationDate)
  val maskedCardNumber = CardNumber("1234-****-****-*456")
  val maskedCard = Card(ucid, maskedCardNumber, CardCvv("***"), CardExpirationDate("**\\**"))
  val cardOperations = List(
    Operation(clientId, 300),
    Operation(clientId, 500),
    Operation(clientId, 800)
  )
  val allOperationStats = OperationStats(
    income = 1300,
    outcome = -1300,
    cardOperations = List(
      CardOperationStats(maskedCardNumber, 1600, 0)
    )
  )

  val emptyOperationStats = OperationStats(
    income = 0,
    outcome = 0,
    cardOperations = List(
      CardOperationStats(maskedCardNumber, 0, 0)
    )
  )

}
