trait DLinkedList[+T] {
  def value: T
  def prev: DLinkedList[T]
  def next: DLinkedList[T]
  def updatedPrev[S >: T](p: => DLinkedList[S]): DLinkedList[S]
  def updatedNext[S >: T](n: => DLinkedList[S]): DLinkedList[S]
  def append[S >: T](element: S): DLinkedList[S]
  def prepend[S >: T](element: S): DLinkedList[S]
}

object Empty extends DLinkedList[Nothing]{
  override def value: Nothing = throw new NoSuchElementException("value of empty list")
  override def prev: DLinkedList[Nothing] = throw new NoSuchElementException
  override def next: DLinkedList[Nothing] = throw new NoSuchElementException

  override def append[S >: Nothing](element: S): DLinkedList[S] = new Cons(
    element, Empty, Empty
  )

  override def updatedPrev[S >: Nothing](p: => DLinkedList[S]): DLinkedList[Nothing] = this

  override def updatedNext[S >: Nothing](n: => DLinkedList[S]): DLinkedList[Nothing] = this

  override def prepend[S >: Nothing](element: S): DLinkedList[S] = append(element)
}

class Cons[+T](override val value: T,
               p: => DLinkedList[T],
               n: => DLinkedList[T]
              ) extends DLinkedList[T]{
  override lazy val prev: DLinkedList[T] = p
  override lazy val next: DLinkedList[T] = n

  override def updatedPrev[S >: T](p: => DLinkedList[S]): DLinkedList[S] = {
    lazy val result = new Cons(value, p.updatedNext(result), next.updatedPrev(result))
    result
  }

  override def updatedNext[S >: T](n: => DLinkedList[S]): DLinkedList[S] = {
    lazy val result = new Cons(value, prev.updatedNext(result), n.updatedPrev(result))
    result
  }

  override def append[S >: T](element: S): DLinkedList[S] = {
    lazy val result = new Cons(element, prev.updatedNext(result), Empty)
    result
  }

  override def prepend[S >: T](element: S): DLinkedList[S] = {
    lazy val result = new Cons(element, Empty, next.updatedPrev(result))
    result
  }
}
