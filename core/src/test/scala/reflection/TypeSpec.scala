package reflection

import testing.BaseSpec

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.{NoSymbol, _}

object TypeSpec {

  def getType[T: TypeTag](obj: T): universe.Type = typeOf[T]
  class A {
    def method(): Int = 1
  }
  class B extends A
  type IntList = List[Int]

}

final class TypeSpec extends BaseSpec {
  import TypeSpec._

  "typeOf" in {

    val IntListType: universe.Type = typeOf[List[Int]]
    IntListType.toString shouldBe "List[Int]"

  }

  "standard types" in {

    universe.definitions.IntTpe shouldBe typeOf[Int]
    universe.definitions.IntClass shouldBe symbolOf[Int]

  }

  "subtyping" in {

    (typeOf[A] <:< typeOf[B]) shouldBe false
    (typeOf[B] <:< typeOf[A]) shouldBe true

    (typeOf[Int] <:< typeOf[Double]) shouldBe false
    (typeOf[Double] <:< typeOf[Int]) shouldBe false

  }

  "weak conformance" in {

    (typeOf[A] weak_<:< typeOf[B]) shouldBe false
    (typeOf[B] weak_<:< typeOf[A]) shouldBe true

    (typeOf[Int] weak_<:< typeOf[Double]) shouldBe true
    (typeOf[Double] weak_<:< typeOf[Int]) shouldBe false

  }

  "equality" in {

    val a1 = new A
    val a2 = new A

    (getType(a1) =:= getType(a2)) shouldBe true

    (getType(List(1, 2, 3)) =:= getType(List(1.0, 2.0, 3.0))) shouldBe false
    (getType(List(1, 2, 3)) =:= getType(List(4, 5, 6))) shouldBe true

    (typeOf[IntList] =:= getType(List(1, 2, 3))) shouldBe true
    (typeOf[IntList] == getType(List(1, 2, 3))) shouldBe false

  }

  "query" in {

    typeOf[List[_]].member(TermName("map")) should not be NoSymbol

    typeOf[B].member(TermName("method")) should not be NoSymbol
    typeOf[B].decl(TermName("method")) shouldBe NoSymbol

  }

}
