package collection.custom.rna

import testing.BaseSpec

private final class RNA1Spec extends BaseSpec {
  import Base._

  "fromSeq" in {

    val xs = List(A, G, U, A)
    val ys = RNA1.fromSeq(xs)
    ys shouldBe a[RNA1]
    ys shouldBe RNA1(A, G, U, A)

  }

  "apply" in {

    val xs = RNA1(A, U, G, G, C)
    xs shouldBe Seq(A, U, G, G, C)

  }

  "take" in {

    val xs = RNA1(A, U, G, G, C)
    val ys = xs.take(3)
    ys shouldBe a[RNA1]
    ys shouldBe RNA1(A, U, G)

  }

  "filter" in {

    val xs = RNA1(A, U, G, G, C)
    val ys = xs.filter(_ != U)
    ys shouldBe RNA1(A, G, G, C)

  }

  "map" in {

    val xs = RNA1(A, U, G, G, C)
    val ys = xs.map {
      case A => U
      case b => b
    }
    ys shouldBe a[IndexedSeq[_]]

  }

  "++" in {

    val xs = RNA1(A, U, G, C)
    (xs ++ xs) shouldBe a[IndexedSeq[_]]

  }

}
