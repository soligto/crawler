package crawler

trait Parser[A] {
  def parse(bytes: Array[Byte]): Either[CrawlerError, (Seq[A], Parser[A])]
}
