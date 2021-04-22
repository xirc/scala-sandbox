package collection.custom.capped

import scala.collection.{
  IndexedSeqView,
  IterableFactory,
  IterableFactoryDefaults,
  IterableOps,
  StrictOptimizedIterableOps,
  View,
  immutable,
  mutable
}

private final class Capped[A] private (
    val capacity: Int,
    val length: Int,
    offset: Int,
    elems: Array[Any]
) extends immutable.Iterable[A]
    with IterableOps[A, Capped, Capped[A]]
    with IterableFactoryDefaults[A, Capped]
    with StrictOptimizedIterableOps[A, Capped, Capped[A]] { self =>

  def this(capacity: Int) =
    this(capacity, length = 0, offset = 0, elems = Array.ofDim(capacity))

  def appended[B >: A](elem: B): Capped[B] = {
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
    new Capped[B](capacity, newLength, newOffset, newElems)
  }

  @inline
  def :+[B >: A](elem: B): Capped[B] = appended(elem)

  def apply(i: Int): A = elems((i + offset) % capacity).asInstanceOf[A]

  override def iterator: Iterator[A] = view.iterator

  override val iterableFactory: IterableFactory[Capped] =
    new CappedFactory(capacity)

  override def view: View[A] = new IndexedSeqView[A] {
    override def apply(i: Int): A = self(i)
    override def length: Int = self.length
  }

  override def className = "Capped"
  override def knownSize: Int = length

}

private class CappedFactory(capacity: Int) extends IterableFactory[Capped] {
  override def from[A](source: IterableOnce[A]): Capped[A] = {
    source match {
      case capped: Capped[A] if capped.capacity == capacity =>
        capped
      case _ =>
        (newBuilder[A] ++= source).result()
    }
  }

  override def empty[A]: Capped[A] = new Capped[A](capacity)

  override def newBuilder[A]: mutable.Builder[A, Capped[A]] =
    new mutable.ImmutableBuilder[A, Capped[A]](empty) {
      override def addOne(elem: A): this.type = {
        elems :+ elem
        this
      }
    }
}
