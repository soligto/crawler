package crawler

/**
 * Парсер массива байт в последовательность элементов A.
 * Может фиксировать промежуточное состояние в результирующем парсере метода parse для парсинга последовательности массивов.
 */
trait Parser[A] {
  def parse(bytes: Array[Byte]): Either[CrawlerError, (Seq[A], Parser[A])]
}
