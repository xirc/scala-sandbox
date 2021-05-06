package ch11

import cats.kernel.{BoundedSemilattice, CommutativeMonoid}
import cats.syntax.all._
import testing.BaseSpec

object CaseStudy11 {

  trait GCounter[F[_, _], K, V] {

    def increment(fa: F[K, V])(k: K, v: V)(implicit
        ev: CommutativeMonoid[V]
    ): F[K, V]

    def merge(fa: F[K, V], fb: F[K, V])(implicit
        ev: CommutativeMonoid[F[K, V]]
    ): F[K, V]

    def total(fa: F[K, V])(implicit
        ev: CommutativeMonoid[V]
    ): V

  }

  object GCounter {

    def apply[F[_, _], K, V](implicit
        ev: GCounter[F, K, V]
    ): GCounter[F, K, V] = ev

    implicit def gCounterInstance[F[_, _], K, V](implicit
        kvs: KeyValueStore[F]
    ): GCounter[F, K, V] =
      new GCounter[F, K, V] {
        import KeyValueStore._

        def increment(fa: F[K, V])(k: K, amount: V)(implicit
            ev: CommutativeMonoid[V]
        ): F[K, V] = {
          val value = fa.getOrElse(k, ev.empty) |+| amount
          fa.put(k, value)
        }

        def merge(fa: F[K, V], fb: F[K, V])(implicit
            ev: CommutativeMonoid[F[K, V]]
        ): F[K, V] = {
          fa |+| fb
        }

        def total(fa: F[K, V])(implicit ev: CommutativeMonoid[V]): V =
          fa.values.toVector.combineAll

      }
  }

  trait KeyValueStore[F[_, _]] {

    def put[K, V](fa: F[K, V])(k: K, v: V): F[K, V]

    def get[K, V](fa: F[K, V])(k: K): Option[V]

    def getOrElse[K, V](fa: F[K, V])(k: K, default: => V): V =
      get(fa)(k).getOrElse(default)

    def values[K, V](fa: F[K, V]): List[V]

  }

  object KeyValueStore {

    implicit class KeyValueStoreOps[F[_, _], K, V](fa: F[K, V]) {

      def put(k: K, v: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
        kvs.put(fa)(k, v)

      def get(k: K)(implicit kvs: KeyValueStore[F]): Option[V] =
        kvs.get(fa)(k)

      def getOrElse(k: K, default: => V)(implicit kvs: KeyValueStore[F]): V =
        kvs.getOrElse(fa)(k, default)

      def values(implicit kvs: KeyValueStore[F]): List[V] =
        kvs.values(fa)

    }

    implicit def mapKvs: KeyValueStore[Map] = new KeyValueStore[Map] {

      override def put[K, V](fa: Map[K, V])(k: K, v: V): Map[K, V] =
        fa + (k -> v)

      override def get[K, V](fa: Map[K, V])(k: K): Option[V] =
        fa.get(k)

      override def values[K, V](fa: Map[K, V]): List[V] =
        fa.values.toList

    }

  }

}

final class CaseStudy11 extends BaseSpec {

  "gcounter" in {

    final case class GCounter(counters: Map[String, Int]) {

      def increment(machine: String, amount: Int): GCounter = {
        val value = counters.getOrElse(machine, 0) + amount
        GCounter(counters + (machine -> value))
      }

      def merge(that: GCounter): GCounter = {
        val newCounters =
          that.counters ++
            counters.map { case (k, v) =>
              k -> v.max(that.counters.getOrElse(k, 0))
            }
        GCounter(newCounters)
      }

      def total: Int = counters.values.sum

    }

    var a = GCounter(Map("A" -> 0))
    var b = GCounter(Map("B" -> 0))

    a = a.increment("A", 3)
    b = b.increment("B", 2)
    a.total shouldBe 3
    b.total shouldBe 2

    a = a.merge(b)
    b = b.merge(a)
    a.total shouldBe 5
    b.total shouldBe 5

    a = a.increment("A", 1)
    a.total shouldBe 6
    b.total shouldBe 5

    a = a.merge(b)
    b = b.merge(a)
    a.total shouldBe 6
    b.total shouldBe 6

  }

  "generic gcounter" in {

    final case class GCounter[V](
        counters: Map[String, V]
    ) {

      def increment(machine: String, amount: V)(implicit
          ev: CommutativeMonoid[V]
      ): GCounter[V] = {
        val value =
          counters.getOrElse(machine, ev.empty) |+| amount
        GCounter(counters + (machine -> value))
      }

      def merge(that: GCounter[V])(implicit
          ev: BoundedSemilattice[V]
      ): GCounter[V] = {
        GCounter(counters |+| that.counters)
      }

      def total(implicit ev: CommutativeMonoid[V]): V =
        counters.values.toVector.combineAll

    }

    val mergeEv: BoundedSemilattice[Int] =
      BoundedSemilattice.instance[Int](0, _ max _)
    var a = GCounter(Map("A" -> 0))
    var b = GCounter(Map("B" -> 0))

    a = a.increment("A", 3)
    b = b.increment("B", 2)
    a.total shouldBe 3
    b.total shouldBe 2

    a = a.merge(b)(mergeEv)
    b = b.merge(a)(mergeEv)
    a.total shouldBe 5
    b.total shouldBe 5

    a = a.increment("A", 1)
    a.total shouldBe 6
    b.total shouldBe 5

    a = a.merge(b)(mergeEv)
    b = b.merge(a)(mergeEv)
    a.total shouldBe 6
    b.total shouldBe 6

  }

  "gcounter as a typeclass" in {

    trait GCounter[F[_, _], K, V] {

      def increment(fa: F[K, V])(k: K, v: V)(implicit
          ev: CommutativeMonoid[V]
      ): F[K, V]

      def merge(fa: F[K, V], fb: F[K, V])(implicit
          ev: BoundedSemilattice[V]
      ): F[K, V]

      def total(fa: F[K, V])(implicit
          ev: CommutativeMonoid[V]
      ): V

    }

    object GCounter {

      def apply[F[_, _], K, V](implicit
          ev: GCounter[F, K, V]
      ): GCounter[F, K, V] = ev

      implicit def mapGCounterInstance[K, V]: GCounter[Map, K, V] =
        new GCounter[Map, K, V] {

          def increment(fa: Map[K, V])(k: K, amount: V)(implicit
              ev: CommutativeMonoid[V]
          ): Map[K, V] = {
            val value =
              fa.getOrElse(k, ev.empty) |+| amount
            fa + (k -> value)
          }

          def merge(fa: Map[K, V], fb: Map[K, V])(implicit
              ev: BoundedSemilattice[V]
          ): Map[K, V] = {
            fa |+| fb
          }

          def total(fa: Map[K, V])(implicit ev: CommutativeMonoid[V]): V =
            fa.values.toVector.combineAll

        }
    }

    val mergeEvidence: BoundedSemilattice[Int] =
      BoundedSemilattice.instance[Int](0, _ max _)
    val gcounter = GCounter[Map, String, Int]
    var a = Map("A" -> 0)
    var b = Map("B" -> 0)

    a = gcounter.increment(a)("A", 3)
    b = gcounter.increment(b)("B", 2)
    gcounter.total(a) shouldBe 3
    gcounter.total(b) shouldBe 2

    a = gcounter.merge(a, b)(mergeEvidence)
    b = gcounter.merge(a, b)(mergeEvidence)
    gcounter.total(a) shouldBe 5
    gcounter.total(b) shouldBe 5

    a = gcounter.increment(a)("A", 1)
    gcounter.total(a) shouldBe 6
    gcounter.total(b) shouldBe 5

    a = gcounter.merge(a, b)(mergeEvidence)
    b = gcounter.merge(a, b)(mergeEvidence)
    gcounter.total(a) shouldBe 6
    gcounter.total(b) shouldBe 6

  }

  "gcounter with kvs" in {

    import CaseStudy11._
    val mergeEvidence: CommutativeMonoid[Map[String, Int]] = {
      implicit val ev: BoundedSemilattice[Int] =
        BoundedSemilattice.instance[Int](0, _ max _)
      CommutativeMonoid[Map[String, Int]]
    }
    val gcounter = GCounter[Map, String, Int]
    var a = Map("A" -> 0)
    var b = Map("B" -> 0)

    a = gcounter.increment(a)("A", 3)
    b = gcounter.increment(b)("B", 2)
    gcounter.total(a) shouldBe 3
    gcounter.total(b) shouldBe 2

    a = gcounter.merge(a, b)(mergeEvidence)
    b = gcounter.merge(a, b)(mergeEvidence)
    gcounter.total(a) shouldBe 5
    gcounter.total(b) shouldBe 5

    a = gcounter.increment(a)("A", 1)
    gcounter.total(a) shouldBe 6
    gcounter.total(b) shouldBe 5

    a = gcounter.merge(a, b)(mergeEvidence)
    b = gcounter.merge(a, b)(mergeEvidence)
    gcounter.total(a) shouldBe 6
    gcounter.total(b) shouldBe 6

  }

}
