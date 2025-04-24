package ru.mipt.fp.domain

/**
 * ADT сетевых ошибок клиентов внешних систем
 */
enum NetworkError:
  case NotFoundError
  case BadRequestError
  case TimeoutError
  case NotAvailableError
  case InternalError
