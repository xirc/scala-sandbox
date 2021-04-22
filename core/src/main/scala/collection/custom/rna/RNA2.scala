package collection.custom.rna

import scala.collection.immutable.{IndexedSeq, IndexedSeqOps}
import scala.collection.{View, mutable}

private final class RNA2 private (val groups: Array[Int], val length: Int)
    extends IndexedSeq[Base]
    with IndexedSeqOps[Base, IndexedSeq, RNA2] {

  import RNA2._

  override def apply(i: Int): Base = {
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException
    Base.fromInt(groups(i / N) >> (i % N * S) & M)
  }

  override def className: String = "RNA2"
  override def empty: RNA2 = fromSeq(Seq.empty)
  override protected def fromSpecific(coll: IterableOnce[Base]): RNA2 =
    fromSeq(coll.iterator.toSeq)
  override protected def newSpecificBuilder: mutable.Builder[Base, RNA2] =
    iterableFactory.newBuilder[Base].mapResult(fromSeq)

  // Overloading

  def concat(suffix: IterableOnce[Base]): RNA2 =
    fromSpecific(iterator ++ suffix.iterator)

  @inline def ++(suffix: IterableOnce[Base]): RNA2 = concat(suffix)

  def appended(base: Base): RNA2 =
    fromSpecific(new View.Appended(this, base))

  def appendedAll(suffix: IterableOnce[Base]): RNA2 =
    concat(suffix)

  def prepended(base: Base): RNA2 =
    fromSpecific(new View.Prepended(base, this))

  def prependedAll(prefix: IterableOnce[Base]): RNA2 =
    fromSpecific(prefix.iterator ++ iterator)

  def map(f: Base => Base): RNA2 =
    fromSpecific(new View.Map(this, f))

  def flatMap(f: Base => IterableOnce[Base]): RNA2 =
    fromSpecific(new View.FlatMap(this, f))

}

private object RNA2 {

  private val S = 2
  private val N = 32 / S
  private val M = (1 << S) - 1

  def fromSeq(buf: Seq[Base]): RNA2 = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i <- buf.indices)
      groups(i / N) |= Base.toInt(buf(i)) << (i % N * S)
    new RNA2(groups, buf.length)
  }

  def apply(bases: Base*): RNA2 = fromSeq(bases)

}
