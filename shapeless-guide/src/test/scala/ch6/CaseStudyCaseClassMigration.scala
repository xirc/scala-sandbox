package ch6

import cats.Monoid
import cats.syntax.all._
import shapeless._
import shapeless.labelled.{FieldType, field}
import shapeless.ops.hlist
import testing.BaseSpec

import scala.annotation.nowarn

object CaseStudyCaseClassMigration {

  trait Migration[A, B] {
    def apply(a: A): B
  }

  implicit class MigrationOps[A](a: A) {
    def migrateTo[B](implicit m: Migration[A, B]): B =
      m.apply(a)
  }

  object Migration {

    implicit def genericMigration[
        A,
        B,
        ARepr <: HList,
        BRepr <: HList,
        Common <: HList,
        Added <: HList,
        Unaligned <: HList
    ](implicit
        aGeneric: LabelledGeneric.Aux[A, ARepr],
        bGeneric: LabelledGeneric.Aux[B, BRepr],
        intersection: hlist.Intersection.Aux[ARepr, BRepr, Common],
        @nowarn diff: hlist.Diff.Aux[BRepr, Common, Added],
        monoid: Monoid[Added],
        prepend: hlist.Prepend.Aux[Added, Common, Unaligned],
        align: hlist.Align[Unaligned, BRepr]
    ): Migration[A, B] = new Migration[A, B] {
      override def apply(a: A): B =
        bGeneric.from(
          align(
            prepend(
              monoid.empty,
              intersection(aGeneric.to(a))
            )
          )
        )
    }

  }

  implicit val hnilMonoid: Monoid[HNil] =
    Monoid.instance(HNil, (_, _) => HNil)

  implicit def hlistMonoid[H, T <: HList](implicit
      hMonoid: Lazy[Monoid[H]],
      tMonoid: Monoid[T]
  ): Monoid[H :: T] = {
    val empty = hMonoid.value.empty :: tMonoid.empty
    Monoid.instance(
      empty,
      (x, y) => {
        val head = hMonoid.value.combine(x.head, y.head)
        val tail = x.tail |+| y.tail
        head :: tail
      }
    )
  }

  implicit def fieldTypeMonoid[K, V](implicit
      monoid: Monoid[V]
  ): Monoid[FieldType[K, V]] =
    Monoid.instance(
      field[K](monoid.empty),
      (x, y) => field[K](monoid.combine(x, y))
    )

}

@nowarn("cat=lint-byname-implicit")
final class CaseStudyCaseClassMigration extends BaseSpec {
  import CaseStudyCaseClassMigration._

  "case study" in {

    case class IceCreamV1(name: String, numCherries: Int, inCone: Boolean)

    // Remove fields
    case class IceCreamV2a(name: String, inCone: Boolean)
    // Reorder fields
    case class IceCreamV2b(name: String, inCone: Boolean, numCherries: Int)
    // Insert fields
    case class IceCreamV2c(
        name: String,
        inCone: Boolean,
        numCherries: Int,
        numWaffles: Int
    )

    val v1 = IceCreamV1("Sundae", 1, true)
    val v2a = IceCreamV2a("Sundae", true)
    val v2b = IceCreamV2b("Sundae", true, 1)
    val v2c = IceCreamV2c("Sundae", true, 1, 0)

    v1.migrateTo[IceCreamV2a] shouldBe v2a
    v1.migrateTo[IceCreamV2b] shouldBe v2b
    v1.migrateTo[IceCreamV2c] shouldBe v2c

  }

  "hnil monoid" in {

    val monoid = Monoid[HNil]
    monoid.combine(HNil, HNil) shouldBe HNil

  }

  "hlist monoid" in {

    val monoid = Monoid[String :: Int :: HNil]
    monoid.combine(
      "a" :: 1 :: HNil,
      "b" :: 2 :: HNil
    ) shouldBe "ab" :: 3 :: HNil

  }

}
