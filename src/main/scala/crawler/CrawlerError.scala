package crawler

import io.circe.{ Decoder, Encoder, HCursor, Json }

sealed trait CrawlerError
case class UnexpectedError(cause: String) extends Exception(cause) with CrawlerError
case class ParserError(cause: String)     extends Exception(cause) with CrawlerError
case class NotFoundError(cause: String)   extends Exception(cause) with CrawlerError
case class BadRequestError(cause: String) extends Exception(cause) with CrawlerError

object CrawlerError {
  implicit val encoder: Encoder[CrawlerError] = (error: CrawlerError) =>
    Json.obj(
      ("type", Json.fromString(error.getClass.getName)),
      (
        "message",
        error match {
          case UnexpectedError(cause) => Json.fromString(cause)
          case ParserError(cause)     => Json.fromString(cause)
          case NotFoundError(cause)   => Json.fromString(cause)
          case BadRequestError(cause) => Json.fromString(cause)
        }
      )
    )

  implicit val decoder: Decoder[CrawlerError] = (c: HCursor) => {
    for {
      error   <- c.downField("type").as[String]
      message <- c.downField("message").as[String]
    } yield error match {
      case "UnexpectedError" => UnexpectedError(message)
      case "ParserError"     => ParserError(message)
      case "NotFoundError"   => NotFoundError(message)
      case "BadRequestError" => BadRequestError(message)
    }
  }
}
