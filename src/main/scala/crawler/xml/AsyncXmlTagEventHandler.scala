package crawler.xml

import crawler.Tag

import scala.collection.immutable.ArraySeq

/**
 * Обработчик событий AsyncXmlStreamParser.
 * Обнаруживает теги с заданным именем и отбирает их с учётом возможной вложенности.
 */
case class AsyncXmlTagEventHandler(
  tag: String,
  finished: Boolean = false,
  private val matchedIndex: Int = 0,               // индекс найденного элемента
  private val currentIndex: Int = 0,               // текущий индекс
  private val buffer: ArraySeq[Byte] = ArraySeq(), // буффер содержимого найденного элемента
  private val parent: Option[AsyncXmlTagEventHandler] =
    None,                                          // родительский handler, если подходящие по названию элементы являются вложенными
  private val results: Vector[Tag] = Vector.empty  // список результатов
) extends AsyncXmlEventHandler[Tag] {

  override def onElementStart(element: String): AsyncXmlTagEventHandler = {
    // если ранее элемент не был найден, и текущий элемент подходит, то устанавливаем matchedIndex и ждём
    // появления содержимого.
    if (matchedIndex == 0) {
      if (element == tag) {
        this.copy(matchedIndex = currentIndex + 1, currentIndex = currentIndex + 1)
      } else {
        this.copy(currentIndex = currentIndex + 1)
      }
    } else {
      // если элемент уже был ранее найден, то создаём дочерний парсер
      if (element == tag) {
        AsyncXmlTagEventHandler(tag, finished, currentIndex + 1, currentIndex + 1, ArraySeq(), Some(this))
      } else {
        this.copy(currentIndex = currentIndex + 1)
      }
    }
  }

  override def onElementEnd(element: String): AsyncXmlTagEventHandler = {
    if (matchedIndex == 0) {
      this.copy(currentIndex = currentIndex - 1)
    } else {
      if (element == tag) {
        if (matchedIndex == currentIndex) {
          // если парсинг искомого элемента закончен, то сбрасываем индекс и помещаем буффер в результат
          if (parent.isEmpty) {
            this.copy(
              finished = true,
              matchedIndex = 0,
              currentIndex = currentIndex - 1,
              results = Tag(new String(buffer.toArray)) +: results
            )
          } else {
            // если есть родительский парсер, то возвращаем его, копируя в список его результатов текущий буффер
            parent.get.copy(
              finished = true,
              currentIndex = currentIndex - 1,
              results = Tag(new String(buffer.toArray)) +: results
            )
          }
        } else {
          this.copy(currentIndex = currentIndex - 1)
        }
      } else {
        this.copy(currentIndex = currentIndex - 1)
      }
    }
  }

  override def onCharacters(text: String): AsyncXmlTagEventHandler = {
    if (matchedIndex == currentIndex)
      this.copy(buffer = buffer ++ text.getBytes)
    else
      this
  }

  override def result: (Vector[Tag], AsyncXmlTagEventHandler) = {
    results match {
      case empty @ Vector() => (empty, this)
      case nonempty         => (nonempty, this.copy(results = Vector.empty))
    }
  }
}
