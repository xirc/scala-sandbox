package ch1

import testing.BaseSpec

object TypeClassBasicSpec {

  // Simple JSON AST
  sealed trait Json

  final case class JsObject(underlying: Map[String, Json]) extends Json

  final case class JsString(underlying: String) extends Json

  final case class JsNumber(underlying: Double) extends Json

  case object JsNull extends Json

  // Type Class
  trait JsonWriter[A] {
    def write(value: A): Json
  }

  object JsonWriter {
    // Type Class Instances
    implicit val stringWriter: JsonWriter[String] = new JsonWriter[String] {
      override def write(value: String): Json = JsString(value)
    }

    implicit def optionWriter[A](implicit
        writer: JsonWriter[A]
    ): JsonWriter[Option[A]] = new JsonWriter[Option[A]] {
      override def write(value: Option[A]): Json = value match {
        case Some(aValue) => writer.write(aValue)
        case None         => JsNull
      }
    }
  }

  // Type Class Use
  // Interface Object
  object Json {
    def toJson[A](value: A)(implicit w: JsonWriter[A]): Json = {
      w.write(value)
    }
  }

  // Type Class Use
  // Interface Syntax
  object JsonSyntax {
    implicit class JsonWriterOps[A](val value: A) extends AnyVal {
      def toJson(implicit w: JsonWriter[A]): Json = {
        w.write(value)
      }
    }
  }

  final case class Person(name: String, email: String)

  object Person {
    // Type Class Instances
    implicit val personWriter: JsonWriter[Person] = new JsonWriter[Person] {
      override def write(value: Person): Json =
        JsObject(
          Map(
            "name" -> JsString(value.name),
            "email" -> JsString(value.email)
          )
        )
    }
  }

}

final class TypeClassBasicSpec extends BaseSpec {
  import TypeClassBasicSpec._

  "use interface object" in {
    val json = Json.toJson(Person("Dave", "dave@example.com"))
    json shouldBe JsObject(
      Map(
        "name" -> JsString("Dave"),
        "email" -> JsString("dave@example.com")
      )
    )
  }

  "use syntax interface" in {
    import JsonSyntax._
    val json = Person("Dave", "dave@example.com").toJson
    json shouldBe JsObject(
      Map(
        "name" -> JsString("Dave"),
        "email" -> JsString("dave@example.com")
      )
    )
  }

  "resolve recursively" in {
    import JsonSyntax._
    val someJson = Option(Person("Dave", "dave@example.com")).toJson
    someJson shouldBe JsObject(
      Map(
        "name" -> JsString("Dave"),
        "email" -> JsString("dave@example.com")
      )
    )
    val noneJson = Option[Person](null).toJson
    noneJson shouldBe JsNull
  }

}
