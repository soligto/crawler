package crawler.xml

import com.fasterxml.aalto.WFCException
import crawler.{ CrawlerError, ParserError, UnexpectedError }

import scala.annotation.tailrec

/**
 * Изначальная идея заключалась в использовании асинхронного парсера FasterXML / aalto-xml.
 * Однако я не учёл, что он не всегда сможет распарсить реальный html, который может являться невалидным xml.
 *
 * Тем не менее, работает это так, что AsyncXmlStreamReader парсит XML по мере загрузки response body
 * и с помощью Cursor'a позволяет обрабатывать наступаемые события в AsyncXmlParser.
 *
 * В итоге реализовал более простое решение в TagParserPipe, но этот код решил оставить.
 */
trait AsyncXmlParser[A] {
  type Self = AsyncXmlParser[A]
  def onElementStart(cursor: Cursor): Self
  def onElementEnd(cursor: Cursor): Self
  def onCharacters(cursor: Cursor): Self
  def finished: Boolean
  def result: (Seq[A], Self)

  def parse(cursor: Cursor): Either[CrawlerError, Self] = {
    @tailrec def go(cursor: Cursor, parser: Self): Self = {
      if (parser.finished) {
        parser
      } else {
        cursor.next match {
          case Finished     => parser
          case StartElement => go(cursor, parser.onElementStart(cursor))
          case EndElement   => go(cursor, parser.onElementEnd(cursor))
          case Characters   => go(cursor, parser.onCharacters(cursor))
        }
      }
    }

    try {
      Right(go(cursor, this))
    } catch {
      case e: WFCException => Left(ParserError(e))
      case e: Exception    => Left(UnexpectedError(e))
    }
  }
}
