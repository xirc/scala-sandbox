package ch5

import shapeless.labelled.FieldType
import shapeless.{::, HList, HNil, LabelledGeneric, Lazy, Witness}
import testing.BaseSpec

import scala.annotation.nowarn

object DerivingInstancesWithLabelledGeneric {

  sealed trait JsonValue extends Product with Serializable
  final case class JsonObject(fields: Map[String, JsonValue]) extends JsonValue
  final case class JsonArray(items: Seq[JsonValue]) extends JsonValue
  final case class JsonString(value: String) extends JsonValue
  final case class JsonNumber(value: Double) extends JsonValue
  final case class JsonBoolean(value: Boolean) extends JsonValue
  case object JsonNull extends JsonValue

  trait JsonEncoder[A] {
    def encode(value: A): JsonValue
  }

  object JsonEncoder {

    def apply[A](implicit encoder: JsonEncoder[A]): JsonEncoder[A] =
      encoder

    def instance[A](f: A => JsonValue): JsonEncoder[A] =
      new JsonEncoder[A] {
        override def encode(value: A): JsonValue = f(value)
      }

    implicit val stringEncoder: JsonEncoder[String] =
      instance(s => JsonString(s))

    implicit val doubleEncoder: JsonEncoder[Double] =
      instance(n => JsonNumber(n))

    implicit val intEncoder: JsonEncoder[Int] =
      instance(n => JsonNumber(n))

    implicit val booleanEncoder: JsonEncoder[Boolean] =
      instance(b => JsonBoolean(b))

    implicit def seqEncoder[A](implicit
        encoder: JsonEncoder[A]
    ): JsonEncoder[Seq[A]] =
      instance(xs => JsonArray(xs.map(encoder.encode)))

    implicit def optionEncoder[A](implicit
        encoder: JsonEncoder[A]
    ): JsonEncoder[Option[A]] =
      instance(opt => opt.map(encoder.encode).getOrElse(JsonNull))

    implicit def genericObjectEncoder[A, H](implicit
        generic: LabelledGeneric.Aux[A, H],
        hEncoder: Lazy[JsonEncoder[H]]
    ): JsonEncoder[A] = {
      instance(value => hEncoder.value.encode(generic.to(value)))
    }

  }

  trait JsonObjectEncoder[A] extends JsonEncoder[A] {
    def encode(value: A): JsonObject
  }

  object JsonObjectEncoder {

    def apply[A](implicit encoder: JsonObjectEncoder[A]): JsonObjectEncoder[A] =
      encoder

    def instance[A](f: A => JsonObject): JsonObjectEncoder[A] =
      new JsonObjectEncoder[A] {
        override def encode(value: A): JsonObject = f(value)
      }

    implicit val hnilEncoder: JsonObjectEncoder[HNil] =
      instance(_ => JsonObject(Map.empty))

    implicit def hlistObjectEncoder[K <: Symbol, H, T <: HList](implicit
        witness: Witness.Aux[K],
        hEncoder: Lazy[JsonEncoder[H]],
        tEncoder: JsonObjectEncoder[T]
    ): JsonObjectEncoder[FieldType[K, H] :: T] = {
      val fieldName = witness.value.name
      instance { hlist =>
        val head = hEncoder.value.encode(hlist.head)
        val tail = tEncoder.encode(hlist.tail)
        JsonObject(tail.fields + (fieldName -> head))
      }
    }

  }

}

final class DerivingInstancesWithLabelledGeneric extends BaseSpec {
  import DerivingInstancesWithLabelledGeneric._
  import JsonEncoder._
  import JsonObjectEncoder._

  "string" in {

    JsonEncoder[String].encode("abc") shouldBe JsonString("abc")

  }

  "double" in {

    JsonEncoder[Double].encode(1.23) shouldBe JsonNumber(1.23)

  }

  "int" in {

    JsonEncoder[Int].encode(123) shouldBe JsonNumber(123)

  }

  "boolean" in {

    JsonEncoder[Boolean].encode(true) shouldBe JsonBoolean(true)

  }

  "seq" in {

    JsonEncoder[Seq[Int]].encode(Seq(1, 2, 3)) shouldBe JsonArray(
      Seq(1, 2, 3).map(JsonNumber(_))
    )

  }

  "option" in {

    JsonEncoder[Option[Int]].encode(Option(1)) shouldBe JsonNumber(1)
    JsonEncoder[Option[Int]].encode(None) shouldBe JsonNull

  }

  "hnil" in {

    JsonEncoder[HNil].encode(HNil) shouldBe JsonObject(Map.empty)

  }

  "hlist object" in {

    case class ClassA(key: String, value: Int)

    val instance = ClassA("abc", 123)
    val expectedJsonValue: JsonValue = JsonObject(
      Map(
        "key" -> JsonString("abc"),
        "value" -> JsonNumber(123)
      )
    )

    val gen = LabelledGeneric[ClassA]
    @nowarn("cat=lint-byname-implicit")
    val encoder = JsonEncoder[gen.Repr]
    encoder.encode(gen.to(instance)) shouldBe expectedJsonValue

  }

  "product" in {

    case class ClassA(
        name: String,
        amount: Int,
        rate: Double,
        available: Boolean
    )
    val instance: ClassA = ClassA("abc", 1, 2.3, true)

    val expectedJsonValue: JsonValue = JsonObject(
      Map(
        "name" -> JsonString("abc"),
        "amount" -> JsonNumber(1),
        "rate" -> JsonNumber(2.3),
        "available" -> JsonBoolean(true)
      )
    )

    @nowarn("cat=lint-byname-implicit")
    val encoder: JsonEncoder[ClassA] = JsonEncoder[ClassA]
    encoder.encode(instance) shouldBe expectedJsonValue

  }

}
