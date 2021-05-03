package ch1

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.syntax.show._
import testing.BaseSpec

import java.util.Date

final class ShowSpec extends BaseSpec {

  "use default instances" in {

    val showInt = Show.apply[Int]
    val showString = Show.apply[String]

    showInt.show(123) shouldBe "123"
    showString.show("abc") shouldBe "abc"

  }

  "use interface syntax" in {

    123.show shouldBe "123"
    "abc".show shouldBe "abc"

  }

  "define custom instances using plain style" in {

    implicit val dateShow: Show[Date] = new Show[Date] {
      override def show(date: Date): String =
        s"${date.getTime}ms since the epoch."
    }

    val date = new Date()
    date.show shouldBe s"${date.getTime}ms since the epoch."

  }

  "define custom instances by a simplified way" in {

    implicit val dateShow: Show[Date] =
      Show.show(date => s"${date.getTime}ms since the epoch.")

    val date = new Date()
    date.show shouldBe s"${date.getTime}ms since the epoch."

  }

}
