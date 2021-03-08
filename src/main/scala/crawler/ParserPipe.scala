package crawler

import cats.Applicative
import fs2.{ Chunk, Pipe, Pull, Stream }

/**
 * Pipe, преобразующий массив байт в
 * Каждый фрагмент response body поступает в Parser, без ожидания, пока наберётся чанк фрагментов (uncons1).
 * Как только Parser[A] обнаруживает элементы, он отправляет их в стрим для дальнейшей обработки.
 */
object ParserPipe {
  type ByteArrayParserPipe[F[_], A] = Pipe[F, Array[Byte], Either[CrawlerError, A]]

  def apply[F[_]: Applicative, A](parser: Parser[A]): ByteArrayParserPipe[F, A] = {
    def go(parser: Parser[A], stream: Stream[F, Array[Byte]]): Pull[F, Either[CrawlerError, A], Unit] = {
      stream.pull.uncons1.flatMap {
        case None                  => Pull.done
        case Some((bytes, stream)) =>
          parser.parse(bytes) match {
            case Left(error)                      =>
              Pull.output(Chunk(Left(error))) >> Pull.done
            case Right((results, finishedParser)) =>
              Pull.output(Chunk.seq(results.map(Right.apply))) >> go(finishedParser, stream)
          }
      }
    }

    source => go(parser, source).stream
  }
}
