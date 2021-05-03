package reflection

import testing.BaseSpec

import scala.reflect.runtime.universe

object ClassSymbolSpec {

  final class ValueClass(val value: Int) extends AnyVal
  class Class
  trait Trait
  abstract class AbstractClass
  case class CaseClass()
  final class FinalClass()
  sealed class SealedClass
  sealed trait SealedTrait

  val BooleanSymbol: universe.ClassSymbol =
    universe.typeOf[Boolean].typeSymbol.asClass

  val IntSymbol: universe.ClassSymbol =
    universe.typeOf[Int].typeSymbol.asClass

  val BigIntSymbol: universe.ClassSymbol =
    universe.typeOf[BigInt].typeSymbol.asClass

  val ValueClassSymbol: universe.ClassSymbol =
    universe.symbolOf[ValueClass].asClass

  val ClassSymbol: universe.ClassSymbol =
    universe.symbolOf[Class].asClass

  val TraitSymbol: universe.ClassSymbol =
    universe.symbolOf[Trait].asClass

  val AbstractClassSymbol: universe.ClassSymbol =
    universe.symbolOf[AbstractClass].asClass

  val CaseClassSymbol: universe.ClassSymbol =
    universe.symbolOf[CaseClass].asClass

  val FinalClassSymbol: universe.ClassSymbol =
    universe.symbolOf[FinalClass].asClass

  val SealedClassSymbol: universe.ClassSymbol =
    universe.symbolOf[SealedClass].asClass

  val SealedTraitSymbol: universe.ClassSymbol =
    universe.symbolOf[SealedTrait].asClass

}

/** [[scala.reflect.api.Symbols.ClassSymbolApi]]
  */
final class ClassSymbolSpec extends BaseSpec {
  import ClassSymbolSpec._

  "isPrimitive" in {

    IntSymbol.isPrimitive shouldBe true
    BooleanSymbol.isPrimitive shouldBe true
    BigIntSymbol.isPrimitive shouldBe false

  }

  "isNumeric" in {

    BooleanSymbol.isNumeric shouldBe false
    IntSymbol.isNumeric shouldBe true
    BigIntSymbol.isNumeric shouldBe false

  }

  "isDerivedValueClass" in {

    ClassSymbol.isDerivedValueClass shouldBe false
    ValueClassSymbol.isDerivedValueClass shouldBe true

  }

  "isTrait" in {

    ClassSymbol.isTrait shouldBe false
    TraitSymbol.isTrait shouldBe true

  }

  "isCaseClass" in {

    ClassSymbol.isCaseClass shouldBe false
    CaseClassSymbol.isCaseClass shouldBe true

  }

  "isSealed" in {

    ClassSymbol.isSealed shouldBe false
    SealedClassSymbol.isSealed shouldBe true
    SealedTraitSymbol.isSealed shouldBe true

  }

}
