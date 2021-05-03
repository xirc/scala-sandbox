package reflection

import testing.BaseSpec

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe.NoSymbol

object TermSymbolSpec {

  class Class(classParam: Boolean) {
    val value: Int = 1
    var variable: String = ""
    lazy val lazyValue: Boolean = true
    def getterLike: Int = 1
  }

  val ClassType: universe.Type =
    universe.typeOf[Class]

  val ValueSymbol: universe.TermSymbol =
    ClassType.decl(universe.TermName("value")).asTerm

  val VariableSymbol: universe.TermSymbol =
    ClassType.decl(universe.TermName("variable")).asTerm

  val LazyValueSymbol: universe.TermSymbol =
    ClassType.decl(universe.TermName("lazyValue")).asTerm

  val GetterLikeSymbol: universe.MethodSymbol =
    ClassType.decl(universe.TermName("getterLike")).asMethod

  val ClassParamSymbol: universe.TermSymbol =
    ClassType.decl(universe.TermName("classParam")).asTerm

  final case class CaseClass(param: Int)

  val CaseClassType: universe.Type =
    universe.typeOf[CaseClass]

  val CaseClassParamSymbol: universe.TermSymbol =
    CaseClassType.decl(universe.TermName("param")).asTerm

}

final class TermSymbolSpec extends BaseSpec {
  import TermSymbolSpec._

  "isVal" in {

    ValueSymbol.accessed.asTerm.isVal shouldBe true
    VariableSymbol.accessed.asTerm.isVal shouldBe false

  }

  "isVar" in {

    ValueSymbol.accessed.asTerm.isVar shouldBe false
    VariableSymbol.accessed.asTerm.isVar shouldBe true

  }

  "isAccessor" in {

    ValueSymbol.isAccessor shouldBe true
    VariableSymbol.isAccessor shouldBe true

  }

  "isGetter" in {

    ValueSymbol.isGetter shouldBe true
    ValueSymbol.getter.asTerm.isGetter shouldBe true

    VariableSymbol.isGetter shouldBe true
    VariableSymbol.getter.asTerm.isGetter shouldBe true

    GetterLikeSymbol.isGetter shouldBe false

  }

  "isSetter" in {

    ValueSymbol.setter shouldBe NoSymbol
    VariableSymbol.setter.asTerm.isSetter shouldBe true

  }

  "isLazy" in {

    ValueSymbol.isLazy shouldBe false
    VariableSymbol.isLazy shouldBe false
    LazyValueSymbol.isLazy shouldBe true

  }

  "isParamAccessor" in {

    ClassParamSymbol.isParamAccessor shouldBe true
    CaseClassParamSymbol.isParamAccessor shouldBe true

  }

  "isCaseAccessor" in {

    ClassParamSymbol.isCaseAccessor shouldBe false
    CaseClassParamSymbol.isCaseAccessor shouldBe true

  }

}
