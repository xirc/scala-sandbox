package collection

import testing.BaseSpec

/** Array
  *
  * https://www.scala-lang.org/api/current/scala/Array.html
  *  - mutable
  *  - fixed length
  */
final class ArraySpec extends BaseSpec {

  "Array" should {

    "be initialized with 3 elements" in {
      val arr = Array(1, 2, 3)
      arr(0) shouldBe 1
      arr(1) shouldBe 2
      arr(2) shouldBe 3
    }

    "not be accessed with out-of-bounds index" in {
      val arr = Array(1, 2, 3)
      a[ArrayIndexOutOfBoundsException] shouldBe thrownBy {
        arr(3)
      }
      a[ArrayIndexOutOfBoundsException] shouldBe thrownBy {
        arr(-1)
      }
    }

    "be initialized using fill" in {
      val arr = Array.fill(4)(1)
      arr shouldBe Array(1, 1, 1, 1)
    }

    "be initialized using tabulate" in {
      val arr = Array.tabulate(5)(_ + 1)
      arr shouldBe Array(1, 2, 3, 4, 5)
    }

    "be initialized using Range" in {
      val arr = (1 to 5).toArray
      arr shouldBe Array(1, 2, 3, 4, 5)
    }

    "be cloned by another Array" in {
      val arr1 = Array(1, 2, 3)
      val arr2 = arr1.clone()
      arr1 shouldBe arr2
      arr2(1) = 4
      arr1(1) shouldBe 2
      arr2(1) shouldBe 4
    }

    "be copied by another Array" in {
      val arr1 = Array(1, 2, 3)
      val arr2 = Array.ofDim[Int](3)
      Array.copy(arr1, 0, arr2, 0, arr1.length)
      arr2 shouldBe Array(1, 2, 3)
    }

  }

  "2D Array" should {

    "be initialized with rows and columns" in {
      val arr2 = Array.ofDim[String](2, 3)
      for (
        i <- arr2.indices;
        j <- arr2(i).indices
      ) {
        arr2(i)(j) = s"$i-$j"
      }
      arr2(0)(0) shouldBe "0-0"
      arr2(0)(1) shouldBe "0-1"
      arr2(0)(2) shouldBe "0-2"
      arr2(1)(0) shouldBe "1-0"
      arr2(1)(1) shouldBe "1-1"
      arr2(1)(2) shouldBe "1-2"
    }

    "be tabulated" in {
      val arr2 = Array.tabulate(2, 3) { case (i, j) => s"$i-$j" }
      for (i <- arr2.indices) {
        for (j <- arr2(i).indices) {
          arr2(i)(j) shouldBe s"$i-$j"
        }
      }
    }

  }

  "3D Array" should {

    "be initialized with dimensions" in {
      val arr3 = Array.ofDim[String](2, 2, 2)
      for (i <- arr3.indices) {
        for (j <- arr3(i).indices) {
          for (k <- arr3(i)(j).indices) {
            arr3(i)(j)(k) = s"$i-$j-$k"
          }
        }
      }
      arr3(0)(0)(0) shouldBe "0-0-0"
      arr3(0)(0)(1) shouldBe "0-0-1"
      arr3(0)(1)(0) shouldBe "0-1-0"
      arr3(0)(1)(1) shouldBe "0-1-1"
      arr3(1)(0)(0) shouldBe "1-0-0"
      arr3(1)(0)(1) shouldBe "1-0-1"
      arr3(1)(1)(0) shouldBe "1-1-0"
      arr3(1)(1)(1) shouldBe "1-1-1"
    }

  }

}
