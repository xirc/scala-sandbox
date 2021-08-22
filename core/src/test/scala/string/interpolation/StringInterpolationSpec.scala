package string.interpolation

import testing.BaseSpec

/** [[https://docs.scala-lang.org/overviews/core/string-interpolation.html String Interpolation]]
  */
final class StringInterpolationSpec extends BaseSpec {

  "s interpolator" in {
    case class Cookie(name: String)
    val i = 1
    val p = 2.0
    val instance = Cookie("yummy")
    s"$i" shouldBe "1"
    s"$p" shouldBe "2.0"
    s"$instance" shouldBe "Cookie(yummy)"
  }

  "f interpolator" in {
    val i = 1
    val p = 2.0
    val s = "123"
    f"$i%04d" shouldBe "0001"
    f"$p%.3f" shouldBe "2.000"
    f"$p%.1e" shouldBe "2.0e+00"
    f"$s" shouldBe "123"
  }

  "raw interpolator" in {
    val s = raw"12\n3"
    s shouldBe "12\\n3"
  }

  "custom interpolator" in {

    implicit class bangHelper(val sc: StringContext) {
      def bang(args: Any*): String = {
        val strings = sc.parts.iterator
        val expressions = args.iterator
        val buf = new StringBuilder(strings.next())
        while (strings.hasNext) {
          buf.append(
            Option(expressions.next()).map(_.toString).getOrElse("null") + "!"
          )
          buf.append(strings.next())
        }
        buf.toString()
      }
    }

    val i = 0
    val p = 2.0
    val s = "abc"
    val nullptr: AnyRef = null
    bang"hello $i world" shouldBe "hello 0! world"
    bang"hello $p" shouldBe "hello 2.0!"
    bang"$s world" shouldBe "abc! world"
    bang"$nullptr" shouldBe "null!"

  }

}
