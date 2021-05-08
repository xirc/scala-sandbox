package reflection

import testing.BaseSpec

import scala.reflect.runtime.universe

object RuntimeMirrorSpec {

  private class C {
    def x = 2
  }

  private class D {
    val x = 2
    var y = 3
  }

  private case class E(x: Int)

  private object F {
    def x = 2
  }

}

final class RuntimeMirrorSpec extends BaseSpec {
  import RuntimeMirrorSpec._

  "Mirrors (runtime, instance, method)" in {

    val runtimeMirror: universe.Mirror =
      universe.runtimeMirror(getClass.getClassLoader)

    val instanceMirror: universe.InstanceMirror =
      runtimeMirror.reflect(new C)

    val methodX: universe.MethodSymbol =
      universe.typeOf[C].decl(universe.TermName("x")).asMethod
    val methodMirror: universe.MethodMirror =
      instanceMirror.reflectMethod(methodX)

    methodMirror() shouldBe 2

  }

  "FieldMirror" in {

    val runtimeMirror: universe.Mirror =
      universe.runtimeMirror(getClass.getClassLoader)

    val instanceMirror: universe.InstanceMirror =
      runtimeMirror.reflect(new D)

    val fieldX: universe.TermSymbol =
      universe.typeOf[D].decl(universe.TermName("x")).asTerm.accessed.asTerm
    val fieldXMirror: universe.FieldMirror =
      instanceMirror.reflectField(fieldX)

    fieldXMirror.get shouldBe 2
    fieldXMirror.set(3)
    fieldXMirror.get shouldBe 3

    val fieldY: universe.TermSymbol =
      universe.typeOf[D].decl(universe.TermName("y")).asTerm.accessed.asTerm
    val fieldYMirror: universe.FieldMirror =
      instanceMirror.reflectField(fieldY)

    fieldYMirror.get shouldBe 3
    fieldYMirror.set(4)
    fieldYMirror.get shouldBe 4

  }

  "ClassMirror" in {

    val runtimeMirror: universe.Mirror =
      universe.runtimeMirror(getClass.getClassLoader)

    val clazz: universe.ClassSymbol =
      universe.typeOf[E].typeSymbol.asClass
    val classMirror: universe.ClassMirror =
      runtimeMirror.reflectClass(clazz)

    val constructor: universe.MethodSymbol =
      universe.typeOf[E].decl(universe.termNames.CONSTRUCTOR).asMethod
    val constructorMirror: universe.MethodMirror =
      classMirror.reflectConstructor(constructor)

    constructorMirror(2) shouldBe E(2)
    constructorMirror(2, 3) shouldBe E(2)

  }

  "ModuleMirror" in {

    val runtimeMirror: universe.Mirror =
      universe.runtimeMirror(getClass.getClassLoader)

    val objectF: universe.ModuleSymbol =
      universe.typeOf[F.type].termSymbol.asModule
    val moduleMirror: universe.ModuleMirror =
      runtimeMirror.reflectModule(objectF)

    moduleMirror.instance shouldBe F

  }

}
