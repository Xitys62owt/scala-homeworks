package ru.mipt.fp.domain

case class CardOperationStats(cardNumber: CardNumber, income: Double, outcome: Double)

case class OperationStats(
    income: Double,
    outcome: Double,
    cardOperations: List[CardOperationStats]
)
