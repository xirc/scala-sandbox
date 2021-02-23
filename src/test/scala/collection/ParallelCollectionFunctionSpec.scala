package collection

import testing.BaseSpec

import scala.collection.parallel.CollectionConverters._

final class ParallelCollectionFunctionSpec extends BaseSpec {

  "aggregate" in {
    case class Item(name: String, price: Int)
    val items = Vector.tabulate(1000)(i => Item(i.toString, i))
    val totalPrice = items.par.aggregate(0)(_ + _.price, _ + _)
    totalPrice shouldBe (999 * 1000 / 2)
  }

}
