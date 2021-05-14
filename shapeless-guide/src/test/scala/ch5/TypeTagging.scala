package ch5

import shapeless.Witness
import shapeless.labelled.{FieldType, field}
import shapeless.syntax.singleton._
import testing.BaseSpec

import scala.annotation.nowarn

final class TypeTagging extends BaseSpec {

  trait Distance

  "Phantom Type" in {

    @nowarn("cat=unused")
    var distance: Int with Distance = 1.asInstanceOf[Int with Distance]
    assertDoesNotCompile(
      """
        |distance = 2
        |""".stripMargin
    )

  }

  "KeyTag" in {

    @nowarn("cat=unused")
    var distance = "distance" ->> 1
    assertDoesNotCompile(
      """
        |distance = 2
        |""".stripMargin
    )

  }

  "FieldType" in {

    @nowarn("cat=unused")
    var distance = field[Distance](1)
    assertDoesNotCompile(
      """
        |distance = 2
        |""".stripMargin
    )

  }

  "FieldType and Witness" in {

    def getFieldName[K, V](value: FieldType[K, V])(implicit
        witness: Witness.Aux[K]
    ): K = witness.value

    def getFieldValue[K, V](value: FieldType[K, V]): V = value

    val distance = "distance" ->> 1

    getFieldName(distance) shouldBe "distance"
    getFieldValue(distance) shouldBe 1

  }

}
