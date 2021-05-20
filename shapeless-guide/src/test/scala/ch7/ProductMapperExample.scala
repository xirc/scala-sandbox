package ch7

import shapeless.ops.hlist.Mapper
import shapeless.{Generic, HList, Poly, Poly1}
import testing.BaseSpec

object ProductMapperExample {

  trait ProductMapper[A, B, P] {
    def apply(a: A): B
  }

  implicit def genericProductMapper[
      A,
      B,
      P <: Poly,
      ARepr <: HList,
      BRepr <: HList
  ](implicit
      aGen: Generic.Aux[A, ARepr],
      bGen: Generic.Aux[B, BRepr],
      mapper: Mapper.Aux[P, ARepr, BRepr]
  ): ProductMapper[A, B, P] = {
    new ProductMapper[A, B, P] {
      def apply(a: A): B =
        bGen.from(mapper.apply(aGen.to(a)))
    }
  }

  implicit class ProductMapperOps[A](a: A) {
    class Builder[B] {
      def apply[P <: Poly](poly: P)(implicit pm: ProductMapper[A, B, P]): B =
        pm.apply(a)
    }
    def mapTo[B]: Builder[B] = new Builder[B]
  }

}

final class ProductMapperExample extends BaseSpec {

  object conversions extends Poly1 {
    implicit val intCase: Case.Aux[Int, Boolean] =
      at(_ > 0)
    implicit val booleanCase: Case.Aux[Boolean, Int] =
      at(if (_) 1 else 0)
    implicit val stringCase: Case.Aux[String, String] =
      at(identity)
  }

  case class IceCream1(name: String, numCherries: Int, inCone: Boolean)
  case class IceCream2(name: String, hasCherries: Boolean, numCones: Int)

  val iceCream2 = IceCream1("Sundae", 1, false).mapTo[IceCream2](conversions)
  iceCream2 shouldBe IceCream2("Sundae", true, 0)

}
