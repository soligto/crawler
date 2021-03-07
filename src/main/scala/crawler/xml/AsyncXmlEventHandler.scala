package crawler.xml

/**
 * Изначальная идея заключалась в использовании асинхронного парсера FasterXML / aalto-xml.
 * Однако я не учёл, что он не всегда сможет распарсить реальный html, который может являться невалидным xml.
 *
 * Тем не менее, работает это так, что AsyncXmlStreamReader парсит XML по мере загрузки response body
 * и с помощью Cursor'a позволяет обрабатывать наступаемые события в AsyncXmlParser.
 *
 * В итоге реализовал более простое решение в RegexTagParser, но этот код решил оставить для примера такого варианта работы.
 */
trait AsyncXmlEventHandler[A] {
  type Self = AsyncXmlEventHandler[A]
  def onElementStart(element: String): Self
  def onElementEnd(element: String): Self
  def onCharacters(characters: String): Self
  def finished: Boolean
  def result: (Seq[A], Self)
}
