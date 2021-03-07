package crawler.xml

import cats.Applicative
import crawler.CrawlerError
import fs2.{ Chunk, Pipe, Pull, Stream }

/**
 * Pipe, извлекающий объект из стрима массива байт.
 * Каждый фрагмент response body поступает в AsyncXmlStreamReader, без ожидания, пока наберётся чанк фрагментов (uncons1).
 * Как только AsyncParser[A] обнаруживает элементы на основании событий, прочитанных из курсора AsyncXmlStreamReader,
 * AsyncXmlParserPipe отправляет их в стрим для дальнейшей обработки.
 */
object AsyncXmlParserPipe {
  type ByteArrayParserPipe[F[_], A] = Pipe[F, Array[Byte], Either[CrawlerError, A]]

  def apply[F[_]: Applicative, A](
    parser: AsyncXmlParser[A],
    reader: AsyncXmlStreamReader
  ): ByteArrayParserPipe[F, A] = {
    def go(
      parser: AsyncXmlParser[A],
      reader: AsyncXmlStreamReader,
      stream: Stream[F, Array[Byte]]
    ): Pull[F, Either[CrawlerError, A], Unit] = {
      stream.pull.uncons1.flatMap {
        case None                  => Pull.done
        case Some((bytes, stream)) =>
          val cursor = reader.feed(bytes)
          parser.parse(cursor).map(_.result) match {
            case Left(error)                      =>
              Pull.output(Chunk(Left(error))) >> Pull.done
            case Right((results, finishedParser)) =>
              Pull.output(Chunk.seq(results.map(Right.apply))) >> go(finishedParser, reader, stream)
          }
      }
    }

    source => go(parser, reader, source).stream
  }
}
