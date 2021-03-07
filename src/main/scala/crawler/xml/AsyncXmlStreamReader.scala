package crawler.xml

import com.fasterxml.aalto.async.{AsyncByteArrayScanner, AsyncStreamReaderImpl}
import com.fasterxml.aalto.stax.InputFactoryImpl
import com.fasterxml.aalto.{AsyncByteArrayFeeder, AsyncXMLStreamReader}
import javax.xml.stream.XMLStreamConstants

import scala.annotation.tailrec

sealed trait Event
case object StartElement extends Event
case object EndElement   extends Event
case object Characters   extends Event
case object Finished     extends Event

trait Cursor {
  def next: Event
  def getName: String
  def getText: String
}

class XmlCursor(private val reader: AsyncXMLStreamReader[AsyncByteArrayFeeder]) extends Cursor {
  def next: Event = {
    @tailrec def go: Event =
      reader.next() match {
        case XMLStreamConstants.START_ELEMENT => StartElement
        case XMLStreamConstants.END_ELEMENT   => EndElement
        case XMLStreamConstants.CHARACTERS    => Characters
        case XmlCursor.XmlStreamEnd           => Finished
        case _                                => go
      }
    go
  }

  def getName: String = reader.getName.getLocalPart
  def getText: String = reader.getText
}
object XmlCursor {
  val XmlStreamEnd = 257
}

class AsyncXmlStreamReader(encoding: String = "UTF-8") {
  val inputFactory = new InputFactoryImpl
  val cfg          = inputFactory.getNonSharedConfig(null, null, null, false, false)
  cfg.setActualEncoding(encoding)
  cfg.doReportCData(false)
  val reader       = new AsyncStreamReaderImpl[AsyncByteArrayFeeder](new AsyncByteArrayScanner(cfg))
  val cursor       = new XmlCursor(reader)

  def feed(bytes: Array[Byte]): Cursor = {
    reader.getInputFeeder.feedInput(bytes, 0, bytes.length)
    cursor
  }
}
