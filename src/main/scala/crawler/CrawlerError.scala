package crawler

sealed trait CrawlerError
case class UnexpectedError(cause: Throwable) extends CrawlerError
case class ParserError(cause: Throwable)     extends CrawlerError
case class NotFoundError(error: String)      extends CrawlerError

sealed trait WebCrawlerServiceError
case class BadRequestError(error: String) extends Exception(error) with WebCrawlerServiceError
