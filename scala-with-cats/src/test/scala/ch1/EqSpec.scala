package ch1

import cats.Eq
import cats.instances.int._
import cats.instances.long._
import cats.instances.option._
import cats.syntax.eq._
import cats.syntax.option._
import testing.BaseSpec

import java.text.SimpleDateFormat
import java.util.Date

final class EqSpec extends BaseSpec {

  "equals int" in {

    val eqInt = Eq[Int]

    eqInt.eqv(123, 123) shouldBe true
    eqInt.eqv(123, 124) shouldBe false

  }

  "use syntax equals" in {

    (123 eqv 123) shouldBe true
    (123 eqv 124) shouldBe false
    (123 =!= 124) shouldBe true

  }

  "use option" in {

    (Option(1) eqv None) shouldBe false
    (1.some eqv None) shouldBe false
    (1.some =!= None) shouldBe true

  }

  "compare custom types" in {

    implicit val dateEq: Eq[Date] = Eq.instance { (date1, date2) =>
      date1.getTime eqv date2.getTime
    }

    val fmt = new SimpleDateFormat("yyyy/MM/dd")
    val x = fmt.parse("2021/04/11")
    val y = fmt.parse("2021/04/12")

    (x eqv x) shouldBe true
    (x eqv y) shouldBe false

  }

}
