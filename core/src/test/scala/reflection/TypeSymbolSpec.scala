package reflection

import testing.BaseSpec

import scala.reflect.runtime.universe

object TypeSymbolSpec {

  class VarianceClass[A, +B, -C]
  type AliasType = Int
  class HasExistentialClass[A: Numeric]

  val VarianceClassSymbol: universe.ClassSymbol =
    universe.typeOf[VarianceClass[_, _, _]].typeSymbol.asClass

  val AliasTypeSymbol: universe.TypeSymbol =
    universe.symbolOf[AliasType]

}

final class TypeSymbolSpec extends BaseSpec {
  import TypeSymbolSpec._

  "variance" in {

    val A = VarianceClassSymbol.typeParams(0).asType
    val B = VarianceClassSymbol.typeParams(1).asType
    val C = VarianceClassSymbol.typeParams(2).asType

    A.isCovariant shouldBe false
    A.isContravariant shouldBe false

    B.isCovariant shouldBe true
    B.isContravariant shouldBe false

    C.isCovariant shouldBe false
    C.isContravariant shouldBe true

  }

  "isAliasType" in {

    VarianceClassSymbol.isAliasType shouldBe false
    AliasTypeSymbol.isAliasType shouldBe true

  }

}
