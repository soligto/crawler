package crawler

import fs2.Stream.InvariantOps
import fs2.{ Chunk, Pipe, Pull, Stream }

import scala.collection.immutable.ArraySeq
import scala.util.matching.Regex

/**
 * Простой парсер, который ищёт нужный тег с помощью регулярного выражения.
 * Принцип работы такой же, как и в AsyncXmlParserPipe - парсим каждый фрагмент, стараясь найти там нужный tag.
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
