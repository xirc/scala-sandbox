package collection.custom.rna

import scala.collection.mutable
import scala.collection.immutable.{IndexedSeq, IndexedSeqOps}

private final class RNA1 private (val groups: Array[Int], val length: Int)
    extends IndexedSeq[Base]
    with IndexedSeqOps[Base, IndexedSeq, RNA1] {

  import RNA1._

  override def apply(i: Int): Base = {
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException
    Base.fromInt(groups(i / N) >> (i % N * S) & M)
  }

  override def className: String = "RNA1"
  override def empty: RNA1 = fromSeq(Seq.empty)
  override protected def fromSpecific(coll: IterableOnce[Base]): RNA1 =
    fromSeq(coll.iterator.toSeq)
  override protected def newSpecificBuilder: mutable.Builder[Base, RNA1] =
    iterableFactory.newBuilder[Base].mapResult(fromSeq)

}

private object RNA1 {

  private val S = 2
  private val N = 32 / S
  private val M = (1 << S) - 1

  def fromSeq(buf: Seq[Base]): RNA1 = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i <- buf.indices)
      groups(i / N) |= Base.toInt(buf(i)) << (i % N * S)
    new RNA1(groups, buf.length)
  }

  def apply(bases: Base*): RNA1 = fromSeq(bases)

}
