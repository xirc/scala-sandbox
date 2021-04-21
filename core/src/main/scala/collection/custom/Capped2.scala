package collection.custom

import scala.collection.{
  AbstractIterator,
  IterableFactory,
  IterableOps,
  immutable,
  mutable
}

private final class Capped2[A] private (
    val capacity: Int,
    val length: Int,
    offset: Int,
    elems: Array[Any]
) extends immutable.Iterable[A]
    with IterableOps[A, Capped2, Capped2[A]] { self =>

  def this(capacity: Int) =
    this(capacity, length = 0, offset = 0, elems = Array.ofDim(capacity))

  def appended[B >: A](elem: B): Capped2[B] = {
    val newElems = Array.ofDim[Any](capacity)
    Array.copy(elems, 0, newElems, 0, capacity)
    val (newOffset, newLength) = {
      if (length == capacity) {
        newElems(offset) = elem
        ((offset + 1) % capacity, length)
      } else {
        newElems(length) = elem
        (offset, length + 1)
      }
    }
    new Capped2[B](capacity, newLength, newOffset, newElems)
  }

  @inline
  def :+[B >: A](elem: B): Capped2[B] = appended(elem)

  def apply(i: Int): A = elems((i + offset) % capacity).asInstanceOf[A]

  override def iterator: Iterator[A] = new AbstractIterator[A] {
    private var current: Int = 0
    override def hasNext: Boolean = current < self.length
    override def next(): A = {
      val elem = self(current)
      current += 1
      elem
    }
  }

  override def className = "Capped2"
  override val iterableFactory: IterableFactory[Capped2] =
    new Capped2Factory(capacity)
  override protected def fromSpecific(iterable: IterableOnce[A]): Capped2[A] =
    iterableFactory.from(iterable)
  override protected def newSpecificBuilder: mutable.Builder[A, Capped2[A]] =
    iterableFactory.newBuilder
  override def empty: Capped2[A] =
    iterableFactory.empty

}

private class Capped2Factory(capacity: Int) extends IterableFactory[Capped2] {
  override def from[A](source: IterableOnce[A]): Capped2[A] =
    (newBuilder[A] ++= source).result()

  override def empty[A]: Capped2[A] = new Capped2[A](capacity)

  override def newBuilder[A]: mutable.Builder[A, Capped2[A]] =
    new mutable.ImmutableBuilder[A, Capped2[A]](empty) {
      override def addOne(elem: A): this.type = {
        elems :+ elem
        this
      }
    }
}
