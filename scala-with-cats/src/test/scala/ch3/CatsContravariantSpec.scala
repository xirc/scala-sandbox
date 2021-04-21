package ch3

import cats.{Contravariant, Show}
import cats.syntax.contravariant._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class CatsContravariantSpec extends AnyWordSpecLike with Matchers {

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
