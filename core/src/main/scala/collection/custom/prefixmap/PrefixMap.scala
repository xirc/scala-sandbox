package collection.custom.prefixmap

import scala.collection.{
  Factory,
  StrictOptimizedIterableOps,
  immutable,
  mutable
}
import scala.language.implicitConversions

private final class PrefixMap[A]
    extends mutable.Map[String, A]
    with mutable.MapOps[String, A, mutable.Map, PrefixMap[A]]
    with StrictOptimizedIterableOps[
      (String, A),
      mutable.Iterable,
      PrefixMap[A]
    ] {

  private var suffixes: immutable.Map[Char, PrefixMap[A]] = immutable.Map.empty
  private var value: Option[A] = None

  override def get(s: String): Option[A] = {
    if (s.isEmpty) value
    else
      suffixes.get(s(0)).flatMap { m =>
        m.get(s.substring(1))
      }
  }

  def withPrefix(s: String): PrefixMap[A] = {
    if (s.isEmpty) this
    else {
      val leading = s(0)
      suffixes.get(leading) match {
        case None =>
          suffixes = suffixes + (leading -> empty)
        case _ =>
      }
      suffixes(leading).withPrefix(s.substring(1))
    }
  }

  override def iterator: Iterator[(String, A)] = {
    val it1 = value.iterator.map { v => ("", v) }
    val it2 = for {
      (c, m) <- suffixes.iterator
      (s, v) <- m.iterator
    } yield {
      (c +: s, v)
    }
    it1 ++ it2
  }

  override def addOne(elem: (String, A)): PrefixMap.this.type = {
    withPrefix(elem._1).value = Option(elem._2)
    this
  }

  override def subtractOne(elem: String): PrefixMap.this.type = {
    if (elem.isEmpty) {
      value = None
    } else {
      suffixes.get(elem(0)).flatMap(_.remove(elem.substring(1)))
    }
    this
  }

  def map[B](f: ((String, A)) => (String, B)): PrefixMap[B] =
    strictOptimizedMap(PrefixMap.newBuilder, f)

  def flatMap[B](f: ((String, A)) => IterableOnce[(String, B)]): PrefixMap[B] =
    strictOptimizedFlatMap(PrefixMap.newBuilder, f)

  override def concat[B >: A](suffix: IterableOnce[(String, B)]): PrefixMap[B] =
    strictOptimizedConcat(suffix, PrefixMap.newBuilder)

  override def empty: PrefixMap[A] =
    new PrefixMap[A]

  override def clear(): Unit =
    suffixes = immutable.Map.empty

  override protected def fromSpecific(
      coll: IterableOnce[(String, A)]
  ): PrefixMap[A] =
    PrefixMap.from(coll)

  override protected def newSpecificBuilder
      : mutable.Builder[(String, A), PrefixMap[A]] =
    PrefixMap.newBuilder

  override def className: String = "PrefixMap"

}

private object PrefixMap {

  def empty[A]: PrefixMap[A] = new PrefixMap[A]

  def from[A](source: IterableOnce[(String, A)]): PrefixMap[A] = {
    source match {
      case m: PrefixMap[A] => m
      case _ =>
        val builder = newBuilder[A]
        builder ++= source
        builder.result()
    }
  }

  def apply[A](kvs: (String, A)*): PrefixMap[A] =
    from(kvs)

  def newBuilder[A]: mutable.Builder[(String, A), PrefixMap[A]] =
    new mutable.GrowableBuilder[(String, A), PrefixMap[A]](empty)

  implicit def toFactory[A](
      self: this.type
  ): Factory[(String, A), PrefixMap[A]] =
    new Factory[(String, A), PrefixMap[A]] {
      override def fromSpecific(it: IterableOnce[(String, A)]): PrefixMap[A] =
        self.from(it)
      override def newBuilder: mutable.Builder[(String, A), PrefixMap[A]] =
        self.newBuilder
    }

}
