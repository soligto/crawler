package crawler.xml

import com.fasterxml.aalto.async.{ AsyncByteArrayScanner, AsyncStreamReaderImpl }
import com.fasterxml.aalto.stax.InputFactoryImpl
import com.fasterxml.aalto.{ AsyncByteArrayFeeder, AsyncXMLStreamReader }
import crawler.{ CrawlerError, Parser, ParserError }
import javax.xml.stream.XMLStreamConstants

import scala.annotation.tailrec

class AsyncXmlStreamParser[A](eventHandler: AsyncXmlEventHandler[A], reader: AsyncXMLStreamReader[AsyncByteArrayFeeder])
    extends Parser[A] {
  override def parse(bytes: Array[Byte]): Either[CrawlerError, (Seq[A], Parser[A])] = {
    reader.getInputFeeder.feedInput(bytes, 0, bytes.length)
    @tailrec def handle(eh: AsyncXmlEventHandler[A]): AsyncXmlEventHandler[A] =
      if (eh.finished) eh
      else
        reader.next match {
          case XMLStreamConstants.START_ELEMENT => handle(eh.onElementStart(reader.getName.getLocalPart))
          case XMLStreamConstants.END_ELEMENT   => handle(eh.onElementEnd(reader.getName.getLocalPart))
          case XMLStreamConstants.CHARACTERS    => handle(eh.onCharacters(reader.getText))
          case _                                => handle(eh)
        }

    try {
      val (results, handler) = handle(eventHandler).result
      Right((results, new AsyncXmlStreamParser[A](handler, reader)))
    } catch {
      case e: Exception => Left(ParserError(e.getMessage))
    }
  }
}

object AsyncXmlStreamParser {
  val XmlStreamEnd = 257

  def apply[A](eventHandler: AsyncXmlEventHandler[A], encoding: String = "UTF-8"): AsyncXmlStreamParser[A] = {
    val inputFactory = new InputFactoryImpl
    val cfg          = inputFactory.getNonSharedConfig(null, null, null, false, false)
    cfg.setActualEncoding(encoding)
    cfg.doReportCData(false)
    val reader       = new AsyncStreamReaderImpl[AsyncByteArrayFeeder](new AsyncByteArrayScanner(cfg))

    new AsyncXmlStreamParser(eventHandler, reader)
  }
}
