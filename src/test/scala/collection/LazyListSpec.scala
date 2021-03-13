package collection

import testing.BaseSpec

/** [[https://www.scala-lang.org/api/current/scala/collection/immutable/LazyList.html LazyList]]
  */
final class LazyListSpec extends BaseSpec {

  "fibonacci sequence" in {
    def fib(a: Long, b: Long): LazyList[Long] = a #:: fib(b, a + b)
    fib(1, 1).take(7) shouldBe Seq(1, 1, 2, 3, 5, 8, 13)
  }

}
