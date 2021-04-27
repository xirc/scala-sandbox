package collection.custom.ops

import testing.BaseSpec

import scala.collection.generic.IsIterable
import scala.collection.{AbstractIterator, AbstractView, BuildFrom}
import scala.language.implicitConversions

private object Transforming {

  final class Intersperse[Repr, S <: IsIterable[Repr]](
      coll: Repr,
      ev: S
  ) {
    def intersperse[B >: ev.A, That](
        sep: B
    )(implicit bf: BuildFrom[Repr, B, That]): That = {
      val iterableOps = ev(coll)
      bf.fromSpecific(coll)(new AbstractView[B] {
        override def iterator: Iterator[B] = new AbstractIterator[B] {
          private val it = iterableOps.iterator
          private var intersperseNext = false
          override def hasNext: Boolean = intersperseNext || it.hasNext
          override def next(): B = {
            val elem = if (intersperseNext) sep else it.next()
            intersperseNext = !intersperseNext && it.hasNext
            elem
          }
        }
      })
    }
  }

  implicit def intersperseOperation[Repr](coll: Repr)(implicit
      ev: IsIterable[Repr]
  ): Intersperse[Repr, ev.type] = new Intersperse(coll, ev)

}

final class Transforming extends BaseSpec {
  import Transforming._

  // scala.collection.BuildFromLowPriority2.buildFromIterableOps
  val xs: List[Int] = List(1, 2, 3).intersperse(0)
  xs shouldBe a[List[_]]
  xs shouldBe List(1, 0, 2, 0, 3)

  // scala.collection.BuildFrom.buildFromMapOps
  val map: Map[Int, String] =
    Map(1 -> "one", 2 -> "two", 3 -> "three").intersperse(0 -> "zero")
  map shouldBe a[Map[_, _]]
  map shouldBe Map(0 -> "zero", 1 -> "one", 2 -> "two", 3 -> "three")

}
