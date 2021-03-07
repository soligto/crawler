import io.circe.generic.semiauto.{ deriveDecoder, deriveEncoder }
import io.circe.{ Decoder, Encoder }
import org.http4s.Uri

package object crawler {
  import CrawlerError._

  implicit val uriEncoder: Encoder[Uri] = Encoder.encodeString.contramap(_.renderString)
  implicit val uriDecoder: Decoder[Uri] = Decoder.decodeString.map(Uri.unsafeFromString)

  implicit val titleDecoder: Decoder[Title] = deriveDecoder
  implicit val titleDncoder: Encoder[Title] = deriveEncoder

  implicit val uriOrStringEncoder: Encoder[Either[String, Uri]] =
    Encoder.encodeString.contramap(_.fold(identity, _.renderString))

  implicit val uriOrStringDecoder: Decoder[Either[String, Uri]] =
    Decoder.decodeString.map(string =>
      Uri.fromString(string) match {
        case Left(_)    => Left(string)
        case Right(uri) => Right(uri)
      }
    )

  implicit val titleErrorDecoder: Decoder[TitleError] = deriveDecoder
  implicit val titleErrorEncoder: Encoder[TitleError] = deriveEncoder

  implicit val titlesRequestDecoder: Decoder[TitlesRequest] = deriveDecoder
  implicit val titlesRequestEncoder: Encoder[TitlesRequest] = deriveEncoder

  implicit val titlesResponseDecoder: Decoder[TitlesResponse] = deriveDecoder
  implicit val titlesResponseEncoder: Encoder[TitlesResponse] = deriveEncoder

  implicit val encoder: Encoder[BadRequestError] = deriveEncoder
}
