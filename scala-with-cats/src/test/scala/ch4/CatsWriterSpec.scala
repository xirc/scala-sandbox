package ch4

import cats.data.Writer
import cats.instances.vector._
import cats.syntax.all._
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt

final class CatsWriterSpec
    extends AnyWordSpecLike
    with Matchers
    with ScalaFutures {

  type Logged[A] = Writer[Vector[String], A]

  "pure" in {

    123.pure[Logged] shouldBe Writer(Vector(), 123)

  }

  "tell" in {

    Vector("1", "2", "3").tell shouldBe Writer(Vector("1", "2", "3"), ())

  }

  "writer" in {

    1.writer(Vector("A", "B")) shouldBe Writer(Vector("A", "B"), 1)

  }

  "extract" in {

    val w = 1.writer(Vector("A", "B"))
    w.value shouldBe 1
    w.written shouldBe Vector("A", "B")
    w.run shouldBe ((Vector("A", "B"), 1))

  }

  "composing & transforming" in {

    val w = for {
      a <- 1.pure[Logged]
      _ <- Vector("a", "b").tell
      b <- 2.writer(Vector("c"))
    } yield a + b
    w.run shouldBe ((Vector("a", "b", "c"), 3))

    val w2 = w.mapWritten(_.map(_.toUpperCase))
    w2.run shouldBe ((Vector("A", "B", "C"), 3))

    val w3 = w.bimap(
      log => log.map(_.toUpperCase),
      res => res * 10
    )
    w3.run shouldBe ((Vector("A", "B", "C"), 30))

    val w4 = w.mapBoth { (log, res) =>
      (log.map(_ + "!"), res * 20)
    }
    w4.run shouldBe ((Vector("a!", "b!", "c!"), 60))

    val w5 = w.reset
    w5.run shouldBe ((Vector(), 3))

    val w6 = w.swap
    w6.run shouldBe ((3, Vector("a", "b", "c")))

  }

  "exercise" in {

    def slowly[A](body: => A): A = {
      try body
      finally Thread.sleep(100)
    }

    def factorial(n: Int): Logged[Int] = {
      for {
        ans <- slowly {
          if (n == 0)
            1.pure[Logged]
          else
            factorial(n - 1).map(n * _)
        }
        _ <- Vector(s"fact $n $ans").tell
      } yield {
        ans
      }
    }

    val rs = Await.result(
      Future.sequence(
        Vector(
          Future(factorial(5)),
          Future(factorial(5))
        )
      ),
      5.seconds
    )

    rs(0).run._1 shouldBe Vector(
      "fact 0 1",
      "fact 1 1",
      "fact 2 2",
      "fact 3 6",
      "fact 4 24",
      "fact 5 120"
    )

  }

}
