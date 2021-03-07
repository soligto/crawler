package crawler

import org.http4s.Uri

case class Title(uri: Uri, title: String)
case class TitleError(uri: Either[String, Uri], message: String, error: Option[CrawlerError])

case class TitlesRequest(uris: Vector[String])
case class TitlesResponse(titles: Vector[Title], errors: Vector[TitleError])
