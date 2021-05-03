package ch3

import cats.syntax.contravariant._
import cats.{Contravariant, Show}
import testing.BaseSpec

final class CatsContravariantSpec extends BaseSpec {

  "contramap" in {

    val showString = Show[String]
    val showSymbol = Contravariant[Show].contramap(showString)((sym: Symbol) =>
      s"'${sym.name}"
    )

    showSymbol.show(Symbol("dave")) shouldBe "'dave"

    val showSymbol2 = showString.contramap[Symbol](sym => s"'${sym.name}")
    showSymbol2.show(Symbol("dave")) shouldBe "'dave"

  }

}
