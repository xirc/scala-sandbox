package collection.custom.rna

import scala.collection.immutable.{IndexedSeq, IndexedSeqOps}
import scala.collection.{
  AbstractIterator,
  SpecificIterableFactory,
  StrictOptimizedIterableOps,
  mutable
}

private final class RNA private (val groups: Array[Int], val length: Int)
    extends IndexedSeq[Base]
    with IndexedSeqOps[Base, IndexedSeq, RNA]
    with StrictOptimizedIterableOps[Base, IndexedSeq, RNA] { self =>

  import RNA._

  override def apply(i: Int): Base = {
    if (i < 0 || i >= length)
      throw new IndexOutOfBoundsException
    Base.fromInt(groups(i / N) >> (i % N * S) & M)
  }

  override def className: String = "RNA"
  override def empty: RNA = RNA.empty
  override protected def fromSpecific(coll: IterableOnce[Base]): RNA =
    RNA.fromSpecific(coll)
  override protected def newSpecificBuilder: mutable.Builder[Base, RNA] =
    RNA.newBuilder

  // Overloading

  def concat(suffix: IterableOnce[Base]): RNA =
    strictOptimizedConcat(suffix, newSpecificBuilder)

  @inline def ++(suffix: IterableOnce[Base]): RNA = concat(suffix)

  def appended(base: Base): RNA = {
    val builder = newSpecificBuilder
    builder ++= this
    builder += base
    builder.result()
  }

  def appendedAll(suffix: IterableOnce[Base]): RNA =
    strictOptimizedConcat(suffix, newSpecificBuilder)

  def prepended(base: Base): RNA = {
    val builder = newSpecificBuilder
    builder += base
    builder ++= this
    builder.result()
  }

  def prependedAll(prefix: IterableOnce[Base]): RNA = {
    val builder = newSpecificBuilder
    builder ++= prefix
    builder ++= this
    builder.result()
  }

  def map(f: Base => Base): RNA =
    strictOptimizedMap(newSpecificBuilder, f)

  def flatMap(f: Base => IterableOnce[Base]): RNA =
    strictOptimizedFlatMap(newSpecificBuilder, f)

  override def iterator: Iterator[Base] = new AbstractIterator[Base] {
    private var i = 0
    private var b = 0
    override def hasNext: Boolean = i < self.length
    override def next(): Base = {
      b = if (i % N == 0) groups(i / N) else b >> S
      i += 1
      Base.fromInt(b & M)
    }
  }

}

private object RNA extends SpecificIterableFactory[Base, RNA] {

  private val S = 2
  private val N = 32 / S
  private val M = (1 << S) - 1

  def fromSeq(buf: scala.collection.Seq[Base]): RNA = {
    val groups = new Array[Int]((buf.length + N - 1) / N)
    for (i <- buf.indices)
      groups(i / N) |= Base.toInt(buf(i)) << (i % N * S)
    new RNA(groups, buf.length)
  }

  override def empty: RNA = fromSeq(Seq.empty)

  override def newBuilder: mutable.Builder[Base, RNA] =
    mutable.ArrayBuffer.newBuilder[Base].mapResult(fromSeq)

  override def fromSpecific(it: IterableOnce[Base]): RNA = it match {
    case seq: Seq[Base] => fromSeq(seq)
    case _              => fromSeq(mutable.ArrayBuffer.from(it))
  }

}
