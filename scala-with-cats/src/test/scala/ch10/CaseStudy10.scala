package ch10

import cats.Semigroup
import cats.data.{Kleisli, NonEmptyList, Validated}
import cats.data.Validated._
import cats.instances.all._
import cats.syntax.all._
import testing.BaseSpec

object CaseStudy10 {

  sealed trait Predicate[E, A] {
    import Predicate._

    def apply(value: A)(implicit ev: Semigroup[E]): Validated[E, A] =
      this match {
        case Pure(f) =>
          f(value)
        case And(left, right) =>
          (left(value), right(value)).mapN((_, _) => value)
        case Or(left, right) =>
          left(value).findValid(right(value))
      }

    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)

    def run()(implicit ev: Semigroup[E]): A => Either[E, A] =
      (a: A) => apply(a).toEither

  }

  object Predicate {

    final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]

    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A])
        extends Predicate[E, A]

    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A])
        extends Predicate[E, A]

    def pure[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

    def lift[E, A](error: E, f: A => Boolean): Predicate[E, A] = Pure { a =>
      if (f(a)) a.valid else error.invalid
    }

  }

}

final class CaseStudy10 extends BaseSpec {
  import CaseStudy10._

  val predA: Predicate[List[String], Int] = Predicate.pure { v =>
    if (v > 2) v.valid
    else List("Must be > 2").invalid
  }

  val predB: Predicate[List[String], Int] = Predicate.pure { v =>
    if (v < -2) v.valid
    else List("Must be < -2").invalid
  }

  "and" in {

    val pred: Predicate[List[String], Int] = predA and predB

    pred(3) shouldBe Invalid(List("Must be < -2"))
    pred(1) shouldBe Invalid(List("Must be > 2", "Must be < -2"))

  }

  "or" in {

    val pred: Predicate[List[String], Int] = predA or predB

    pred(3) shouldBe Valid(3)
    pred(1) shouldBe Invalid(List("Must be > 2", "Must be < -2"))

  }

  type Errors = NonEmptyList[String]

  def error(message: String): NonEmptyList[String] = NonEmptyList(message, Nil)

  def longerThan(n: Int): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be longer than $n characters"),
      string => string.length > n
    )

  val alphanumeric: Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must be all alphanumeric characters"),
      string => string.forall(_.isLetterOrDigit)
    )

  def contains(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char"),
      string => string.contains(char)
    )

  def containOnce(char: Char): Predicate[Errors, String] =
    Predicate.lift(
      error(s"Must contain the character $char only once"),
      string => string.count(_ == char) == 1
    )

  "username" in {

    val username =
      Kleisli((longerThan(3) and alphanumeric).run())

    username("user1") shouldBe "user1".asRight
    username("a").isLeft shouldBe true
    username("user1!").isLeft shouldBe true

  }

  "email" in {

    val checkName: Predicate[Errors, String] =
      longerThan(0)

    val checkDomain: Predicate[Errors, String] =
      longerThan(3) and contains('.')

    val email =
      Kleisli((string: String) =>
        string.split('@') match {
          case Array(name, domain) =>
            checkName(name) |+| checkDomain(domain)
          case _ =>
            error("Must contain a single @ character").invalid
        }
      )

    email("user@example.com").isValid shouldBe true
    email("user@app.example.com").isValid shouldBe true
    email("@example.com").isValid shouldBe false
    email("user@example").isValid shouldBe false
    email("user@a.b").isValid shouldBe false
    email("user").isValid shouldBe false

  }

}
