package collection.custom.ops

import testing.BaseSpec

import scala.collection.generic.{IsIterable, IsMap}
import scala.collection.{MapOps, SeqOps}
import scala.language.implicitConversions

object Consuming {

  implicit final class SumBy[A](val collection: IterableOnce[A])
      extends AnyVal {
    def sumBy[B: Numeric](f: A => B)(implicit ev: Numeric[B]): B = {
      collection.iterator.foldLeft(ev.zero) { (acc, value) =>
        ev.plus(acc, f(value))
      }
    }
  }

  implicit def sumByOperation[R](coll: R)(implicit
      ev: IsIterable[R]
  ): SumBy[ev.A] = {
    new SumBy[ev.A](ev(coll))
  }

  implicit final class AvgBy[A, C](val collection: SeqOps[A, Iterable, C])
      extends AnyVal {
    def avgBy[B](f: A => B)(implicit ev: Fractional[B]): B = {
      val (count, sum) = collection.view.map(f).foldLeft((0, ev.zero)) {
        case ((count, sum), value) =>
          (count + 1, ev.plus(sum, value))
      }
      ev.div(sum, ev.fromInt(count))
    }
  }

  implicit final class WeightedSum[K, V, C](
      val collection: MapOps[K, V, IsMap.Tupled[Iterable]#Ap, C]
  ) extends AnyVal {

    def weightedSum[A](f: K => A, g: V => A)(implicit
        ev: Fractional[A]
    ): A = {
      val (totalWeight, sum) = collection.foldLeft((ev.zero, ev.zero)) {
        case ((totalWeight, sum), (k, v)) =>
          val w = f(k)
          val s = ev.times(w, g(v))
          (ev.plus(totalWeight, w), ev.plus(sum, s))
      }
      ev.div(sum, totalWeight)
    }

  }

}

final class Consuming extends BaseSpec {
  import Consuming._

  "Iterable" in {

    val tokens = Seq("1", "2", "3")
    tokens.sumBy(_.length) shouldBe 3
    tokens.sumBy(_.toInt) shouldBe 6

    "123456789".sumBy(_.toString.toInt) shouldBe 45

  }

  "Seq" in {

    val xs = Seq(1, 2, 3, 4)
    xs.avgBy(_.toDouble) shouldBe 2.5
    xs.view.avgBy(_.toDouble) shouldBe 2.5

  }

  "Map" in {

    val mp = Map(
      1 -> 3,
      2 -> 1,
      3 -> 2
    )
    mp.weightedSum(_.toDouble, _.toDouble) shouldBe (11.0 / 6.0)

  }

}
