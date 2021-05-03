package ch3

import cats.Monoid
import cats.instances.string._
import cats.syntax.invariant._
import cats.syntax.semigroup._
import testing.BaseSpec

final class CatsInvariantSpec extends BaseSpec {

  implicit val symbolMonoid: Monoid[Symbol] = {
    Monoid[String].imap(Symbol.apply)(_.name)
  }

  assert(Monoid[Symbol].empty === Symbol(""))

  assert(
    (Symbol("a") |+| Symbol("few") |+| Symbol("words")) === Symbol("afewwords")
  )

}
