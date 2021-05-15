package ch6

import shapeless._
import shapeless.record._
import testing.BaseSpec

final class RecordOpsExample extends BaseSpec {

  case class IceCream(name: String, numCherries: Int, inCone: Boolean)
  val generic = LabelledGeneric[IceCream]

  "select" in {

    val sundae = generic.to(IceCream("Sundae", 1, false))
    assert(sundae.get(Symbol("name")) === "Sundae")
    assert(sundae.get(Symbol("numCherries")) === 1)
    assert(sundae.get(Symbol("inCone")) === false)

  }

  "updated" in {

    val sundae = generic.to(IceCream("Sundae", 1, false))

    val sundae2 = sundae.updated(Symbol("numCherries"), 2)
    assert(generic.from(sundae2) === IceCream("Sundae", 2, false))

    val sundae3 = sundae.updated(Symbol("inCone"), true)
    assert(generic.from(sundae3) === IceCream("Sundae", 1, true))

  }

  "remove" in {

    val sundae = generic.to(IceCream("Sundae", 1, false))

    val (_, sundae2) = sundae.remove(Symbol("inCone"))
    val expected = "Sundae" :: 1 :: HNil
    assert(sundae2 === expected)

  }

  "updateWith" in {

    val sundae = generic.to(IceCream("Sundae", 1, false))

    val sundae2 = sundae.updateWith(Symbol("numCherries"))(_ + 2)
    assert(generic.from(sundae2) === IceCream("Sundae", 3, false))

  }

  "toMap" in {

    val sundae = generic.to(IceCream("Sundae", 1, false))
    val expected = Map(
      Symbol("name") -> "Sundae",
      Symbol("numCherries") -> 1,
      Symbol("inCone") -> false
    )
    assert(sundae.toMap === expected)

  }

}
