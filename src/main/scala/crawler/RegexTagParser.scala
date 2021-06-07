package crawler

import scala.collection.immutable.ArraySeq
import scala.util.matching.Regex

/**
 * Простой парсер, который ищёт нужный тег с помощью регулярного выражения.
 */
case class RegexTagParser(tag: Regex, content: ArraySeq[Byte]) extends Parser[Tag] {
  def parse(bytes: Array[Byte]): Either[CrawlerError, (Vector[Tag], RegexTagParser)] = {
    val tagParser = this.copy(content = content ++ bytes)
    Right((tag
      .findAllMatchIn(new String(tagParser.content.toArray))
      .map(m => Tag(m.group(1)))
      .toVector,
      tagParser))
  }
}

object RegexTagParser {
  def apply(tag: String) = new RegexTagParser(s"<$tag>(.*)<\\/$tag>".r, ArraySeq())
}
