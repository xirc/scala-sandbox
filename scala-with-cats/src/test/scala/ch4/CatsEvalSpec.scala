package ch4

import cats.Eval
import testing.BaseSpec

final class CatsEvalSpec extends BaseSpec {

  "now" in {

    var count = 0
    val x = Eval.now {
      count += 1
      math.random()
    }

    count shouldBe 1
    x.value shouldBe x.value
    count shouldBe 1

  }

  "always" in {

    var count = 0
    val y = Eval.always {
      count += 1
      math.random()
    }

    y.value should not be y.value
    count shouldBe 2

  }

  "later" in {

    var count = 0
    val z = Eval.later {
      count += 1
      math.random()
    }

    count shouldBe 0
    z.value shouldBe z.value
    count shouldBe 1

  }

  "eval" in {

    // stack safe
    def factorial(n: BigInt): Eval[BigInt] =
      if (n == 1) {
        Eval.now(n)
      } else {
        Eval.defer(factorial(n - 1)).map(_ * n)
      }

    factorial(50_000).value should be >= BigInt(0)

  }

  "exercise" in {

    def foldRightEval[A, B](xs: List[A], acc: Eval[B])(
        f: (A, Eval[B]) => Eval[B]
    ): Eval[B] = {
      xs match {
        case head :: tail =>
          f(head, foldRightEval(tail, acc)(f))
        case Nil =>
          acc
      }
    }

    def foldRight[A, B](xs: List[A], acc: B)(f: (A, B) => B): B = {
      foldRightEval(xs, Eval.now(acc)) { (a, b) =>
        b.map(f(a, _))
      }.value
    }

    foldRight(List(1, 2, 3), 0)(_ + _) shouldBe 6

  }

}
