package crawler

import fs2.Stream.InvariantOps
import fs2.{ Chunk, Pipe, Pull, Stream }

import scala.collection.immutable.ArraySeq
import scala.util.matching.Regex

/**
 * Простой парсер, который ищёт нужный тег с помощью регулярного выражения.
 */
case class TagParser(tag: Regex, content: ArraySeq[Byte]) {
  def parse(bytes: Array[Byte]): (Option[String], TagParser) = {
    val tagParser = this.copy(content = content ++ bytes)
    (tag.findFirstMatchIn(new String(tagParser.content.toArray)).map(_.group(1)), tagParser)
  }
}

object TagParser {
  def apply(tag: String) = new TagParser(s"<$tag>(.*)<\\/$tag>".r, ArraySeq())
}

/**
 * Pipe, извлекающий объект из стрима массива байт.
 * Каждый фрагмент response body поступает в TagParser, без ожидания, пока наберётся чанк фрагментов (uncons1).
 * Как только TagParser обнаруживает элемент, TagParserPipe отправляет их в стрим для дальнейшей обработки.
 */
object TagParserPipe {
  type ByteArrayParserPipe[F[_], A] = Pipe[F, Array[Byte], Either[CrawlerError, A]]

  def apply[F[_]](tagParser: TagParser): ByteArrayParserPipe[F, Tag] = {
    def go(stream: Stream[F, Array[Byte]], tagParser: TagParser): Pull[F, Either[CrawlerError, Tag], Unit] = {
      stream.pull.uncons1.flatMap {
        case None                  => Pull.done
        case Some((bytes, stream)) =>
          tagParser.parse(bytes) match {
            case (None, newParser)         =>
              go(stream, newParser)
            case (Some(result), newParser) =>
              Pull.output(Chunk(Right(Tag(result)))) >> go(stream, newParser)
          }
      }
    }

    source => go(source, tagParser).stream
  }
}
