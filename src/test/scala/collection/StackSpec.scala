package collection

import testing.BaseSpec

import scala.collection.mutable

/** [[https://www.scala-lang.org/api/current/scala/collection/mutable/Stack.html Stack]]
  *  - mutable
  */
final class StackSpec extends BaseSpec {

  "Stack" should {

    "be initialized with 3 elements" in {
      val stack = mutable.Stack("a", "b", "c")
      stack(0) shouldBe "a"
      stack(1) shouldBe "b"
      stack(2) shouldBe "c"
    }

    "be added elements using push and pushAll" in {
      // NOTE Don't use +=
      val stack = mutable.Stack.empty[Int]
      stack.push(1)
      stack.push(2)
      stack.push(3)
      stack.push(4, 5)
      stack.pushAll(Seq(6, 7))
      stack(0) shouldBe 7
      stack(1) shouldBe 6
      stack(2) shouldBe 5
      stack(3) shouldBe 4
      stack(4) shouldBe 3
      stack(5) shouldBe 2
      stack(6) shouldBe 1
    }

    "be popped elements using pop" in {
      // Don't use += or ++=
      val stack = mutable.Stack.empty[Int]
      stack.push(1)
      stack.push(2)
      stack.push(3, 4)
      stack.pushAll(Seq(5, 6))
      stack.pop() shouldBe 6
      stack.pop() shouldBe 5
      stack.pop() shouldBe 4
      stack.pop() shouldBe 3
      stack.pop() shouldBe 2
      stack.pop() shouldBe 1
    }

    "be popped elements while the condition is met" in {
      val stack = mutable.Stack.empty[Int]
      stack.push(1, 2, 3, 4, 5)
      stack.popWhile(_ > 2) shouldBe Seq(5, 4, 3)
      stack.pop() shouldBe 2
      stack.pop() shouldBe 1
    }

  }

}
