package ch3

import cats.Monoid
import cats.instances.string._
import cats.syntax.invariant._
import cats.syntax.semigroup._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

final class CatsInvariantSpec extends AnyWordSpecLike with Matchers {

  implicit val symbolMonoid: Monoid[Symbol] = {
    Monoid[String].imap(Symbol.apply)(_.name)
  }

  assert(Monoid[Symbol].empty === Symbol(""))

  assert(
    (Symbol("a") |+| Symbol("few") |+| Symbol("words")) === Symbol("afewwords")
  )

}
