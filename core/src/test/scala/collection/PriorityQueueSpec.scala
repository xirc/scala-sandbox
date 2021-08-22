package collection

import testing.BaseSpec

import scala.collection.mutable

/** [[https://www.scala-lang.org/api/current/scala/collection/mutable/PriorityQueue.html PriorityQueue]]
  *   - mutable
  */
final class PriorityQueueSpec extends BaseSpec {

  "PriorityQueue" should {

    "be enqueued elements" in {
      case class Item(name: String, price: Int)
      val queue = mutable.PriorityQueue.empty[Item](Ordering.by(_.price))
      queue.enqueue(Item("toy", 12))
      queue.enqueue(Item("ice", 1))
      queue.enqueue(Item("car", 1000))

      queue.clone.dequeueAll shouldBe Seq(
        Item("car", 1000),
        Item("toy", 12),
        Item("ice", 1)
      )
    }

    "be dequeued elements" in {
      case class Item(name: String, price: Int)
      val queue = mutable.PriorityQueue.empty[Item](Ordering.by(_.price))
      queue.enqueue(Item("toy", 12))
      queue.enqueue(Item("ice", 1))
      queue.enqueue(Item("car", 1000))

      queue.dequeue() shouldBe Item("car", 1000)
      queue.dequeue() shouldBe Item("toy", 12)
      queue.dequeue() shouldBe Item("ice", 1)
    }

    "take a custom implicit ordering" in {
      case class Item(name: String, price: Int)
      implicit val ordering: Ordering[Item] = Ordering.by(Item.unapply(_))

      val queue = mutable.PriorityQueue.empty[Item]
      queue.enqueue(Item("ace", 12))
      queue.enqueue(Item("ace", 2))
      queue.enqueue(Item("bike", 1))
      queue.enqueue(Item("bike", 2))
      queue.enqueue(Item("car", 1000))
      queue.enqueue(Item("car", 2000))

      queue.dequeue() shouldBe Item("car", 2000)
      queue.dequeue() shouldBe Item("car", 1000)
      queue.dequeue() shouldBe Item("bike", 2)
      queue.dequeue() shouldBe Item("bike", 1)
      queue.dequeue() shouldBe Item("ace", 12)
      queue.dequeue() shouldBe Item("ace", 2)
    }

  }

}
