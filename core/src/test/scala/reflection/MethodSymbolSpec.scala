package reflection

import testing.BaseSpec

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.NoSymbol

object MethodSymbolSpec {

  class Class(param: Int) {

    def this() = this(0)

    def method(): Int = param
    def overloadedMethod(value: Int): Int = value
    def overloadedMethod(value: String): Int = value.length
    def varargsMethod(args: Int*): Int = args.sum
    def byNameMethod(value: => Int): Int = value
    def withDefaultMethod(value: Int = 0): Int = value

  }

  val ClassType: universe.Type =
    universe.typeOf[Class]

  val PrimaryConstructorSymbol: universe.MethodSymbol =
    ClassType
      .decl(universe.termNames.CONSTRUCTOR)
      .alternatives
      .find { m =>
        m.asMethod.paramLists.exists(_.size == 1)
      }
      .getOrElse(NoSymbol)
      .asMethod

  val SecondaryConstructorSymbol: universe.MethodSymbol =
    ClassType
      .decl(universe.termNames.CONSTRUCTOR)
      .alternatives
      .find { m =>
        m.asMethod.paramLists.exists(_.isEmpty)
      }
      .getOrElse(NoSymbol)
      .asMethod

  val MethodSymbol: universe.MethodSymbol =
    ClassType.decl(universe.TermName("method")).asMethod

  val OverloadedMethodSymbol: universe.TermSymbol =
    ClassType.decl(universe.TermName("overloadedMethod")).asTerm

  val VarargsMethodSymbol: universe.MethodSymbol =
    ClassType.decl(universe.TermName("varargsMethod")).asMethod

  val ByNameMethodSymbol: universe.MethodSymbol =
    ClassType.decl(universe.TermName("byNameMethod")).asMethod

  val WithDefaultMethodSymbol: universe.MethodSymbol =
    ClassType.decl(universe.TermName("withDefaultMethod")).asMethod

}

final class MethodSymbolSpec extends BaseSpec {
  import MethodSymbolSpec._

  "isPrimaryConstructor" in {

    PrimaryConstructorSymbol.isPrimaryConstructor shouldBe true
    SecondaryConstructorSymbol.isPrimaryConstructor shouldBe false
    MethodSymbol.isPrimaryConstructor shouldBe false

  }

  "isOverloaded" in {

    OverloadedMethodSymbol.isOverloaded shouldBe true

  }

  "isVarargs" in {

    VarargsMethodSymbol.isVarargs

  }

  "isByNameParam" in {

    val byNameParamSymbol =
      ByNameMethodSymbol.paramLists.head.head.asTerm
    byNameParamSymbol.isByNameParam shouldBe true

  }

  "isParamWithDefault" in {

    val withDefaultParamSymbol =
      WithDefaultMethodSymbol.paramLists.head.head.asTerm
    withDefaultParamSymbol.isParamWithDefault shouldBe true

  }

}
