package collection

import testing.BaseSpec

import scala.collection.mutable

/** [[https://www.scala-lang.org/api/current/scala/collection/mutable/Queue.html Queue]]
  *  - mutable
  */
final class QueueSpec extends BaseSpec {

  "Queue" should {

    "be initialized with 3 elements" in {
      val queue = mutable.Queue(1, 2, 3)
      queue(0) shouldBe 1
      queue(1) shouldBe 2
      queue(2) shouldBe 3
      queue.dequeue() shouldBe 1
      queue.dequeue() shouldBe 2
      queue.dequeue() shouldBe 3
    }

    "be enqueued an element" in {
      val queue = mutable.Queue.empty[Int]
      queue.enqueue(1)
      queue.enqueue(2)
      queue(0) shouldBe 1
      queue(1) shouldBe 2
    }

    "be dequeued an element" in {
      val queue = mutable.Queue.empty[Int]
      queue.enqueue(1)
      queue.enqueue(2)
      queue.dequeue() shouldBe 1
      queue.dequeue() shouldBe 2
    }

  }

}
