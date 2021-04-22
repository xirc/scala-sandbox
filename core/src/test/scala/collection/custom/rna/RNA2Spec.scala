package collection.custom.rna

import testing.BaseSpec

private final class RNA2Spec extends BaseSpec {

  import Base._

  "map" in {

    val xs = RNA2(A, U, G, C)
    val ys = xs.map {
      case A => U
      case b => b
    }
    ys shouldBe a[RNA2]
    ys shouldBe RNA2(U, U, G, C)

  }

  "++" in {

    val xs = RNA2(A, U, G, C)
    val ys = xs ++ xs
    ys shouldBe a[RNA2]
    ys shouldBe RNA2(A, U, G, C, A, U, G, C)

  }

}
