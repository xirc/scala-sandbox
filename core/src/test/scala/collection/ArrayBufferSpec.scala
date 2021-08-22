package collection

import testing.BaseSpec

import scala.collection.mutable.ArrayBuffer

/** [[https://www.scala-lang.org/api/current/scala/collection/mutable/ArrayBuffer.html ArrayBuffer]]
  *   - mutable
  *   - variable-length
  */
final class ArrayBufferSpec extends BaseSpec {

  "ArrayBuffer" should {

    "be initialized with 3 elements" in {
      val arr = ArrayBuffer(1, 2, 3)
      arr(0) shouldBe 1
      arr(1) shouldBe 2
      arr(2) shouldBe 3
    }

    "not be accessed with out-of-bounds index" in {
      val arr = ArrayBuffer(1, 2, 3)
      a[IndexOutOfBoundsException] shouldBe thrownBy {
        arr(3)
      }
      a[IndexOutOfBoundsException] shouldBe thrownBy {
        arr(-1)
      }
    }

    "be initialized using fill" in {
      val arr = ArrayBuffer.fill(4)(1)
      arr shouldBe ArrayBuffer(1, 1, 1, 1)
    }

    "be initialized using tabulate" in {
      val arr = ArrayBuffer.tabulate(5)(_ + 1)
      arr shouldBe ArrayBuffer(1, 2, 3, 4, 5)
    }

    "be added an element using +=" in {
      val arr = ArrayBuffer.empty[Int]
      arr += 1
      arr += 2
      arr shouldBe ArrayBuffer(1, 2)
    }

    "be added elements using ++=" in {
      val arr = ArrayBuffer.empty[Int]
      arr ++= Seq(1, 2, 3)
      arr shouldBe ArrayBuffer(1, 2, 3)
    }

    "be cloned by another Array" in {
      val arr1 = ArrayBuffer(1, 2, 3)
      val arr2 = arr1.clone()
      arr1 shouldBe arr2
      arr2(1) = 4
      arr1(1) shouldBe 2
      arr2(1) shouldBe 4
    }

  }

}
