import io.circe.generic.semiauto.deriveCodec
import io.circe.{ Codec, Decoder, Encoder }
import logstage.LogstageCodec
import logstage.circe.LogstageCirceCodec
import org.http4s.Uri

package object crawler {
  implicit val uriEncoder: Encoder[Uri]          = Encoder.encodeString.contramap(_.renderString)
  implicit val uriDecoder: Decoder[Uri]          = Decoder.decodeString.map(Uri.unsafeFromString)
  implicit val logUriDecoder: LogstageCodec[Uri] = LogstageCirceCodec.derived

  implicit val titleCodec: Codec[Title]            = deriveCodec
  implicit val logTitleCodec: LogstageCodec[Title] = LogstageCirceCodec.derived

  implicit val uriOrStringEncoder: Encoder[Either[String, Uri]] =
    Encoder.encodeString.contramap(_.fold(identity, _.renderString))
  implicit val uriOrStringDecoder: Decoder[Either[String, Uri]] =
    Decoder.decodeString.map(string =>
      Uri.fromString(string) match {
        case Left(_)    => Left(string)
        case Right(uri) => Right(uri)
      }
    )
  implicit val logUriOrStringEncoder: LogstageCodec[Either[String, Uri]] = LogstageCirceCodec.derived

  implicit val titleErrorCodec: Codec[TitleError]            = deriveCodec
  implicit val logTitleErrorCodec: LogstageCodec[TitleError] = LogstageCirceCodec.derived

  implicit val titlesRequestCodec: Codec[TitlesRequest]            = deriveCodec
  implicit val logTitlesRequestCodec: LogstageCodec[TitlesRequest] = LogstageCirceCodec.derived

  implicit val titlesResponseCodec: Codec[TitlesResponse]            = deriveCodec
  implicit val logTitlesResponseCodec: LogstageCodec[TitlesResponse] = LogstageCirceCodec.derived
}
