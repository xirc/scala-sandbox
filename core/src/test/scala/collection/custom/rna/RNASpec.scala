package collection.custom.rna

import testing.BaseSpec

final class RNASpec extends BaseSpec {

  import Base._

  "to" in {

    val xs = Seq(A, U, G, C)
    val ys = xs.to(RNA)
    ys shouldBe RNA(A, U, G, C)

  }

}
