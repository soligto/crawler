package crawler.xml

/**
 * Изначальная идея заключалась в использовании асинхронного парсера FasterXML / aalto-xml.
 * Однако я не учёл, что он не всегда сможет распарсить реальный html, который может быть невалидным xml.
 *
 * В итоге реализовал более простое решение в RegexTagParser, но этот код решил оставить для примера такого варианта работы.
 *
 * AsyncXmlEventHandler обрабатывает события, наступающие в AsyncXmlStreamReader по мере разбора XML.
 *
 * При завершении парсинга фрамента данных или при возврате true методом finished AsyncXmlStreamReader забриает доступные результаты
 * через вызов метода result.
 */
trait AsyncXmlEventHandler[A] {
  type Self = AsyncXmlEventHandler[A]
  def onElementStart(element: String): Self
  def onElementEnd(element: String): Self
  def onCharacters(characters: String): Self
  def finished: Boolean
  def result: (Seq[A], Self)
}
