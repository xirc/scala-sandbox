package collection.custom.prefixmap

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

private final class PrefixMapSpec extends AnyWordSpecLike with Matchers {

  val m = PrefixMap("hello" -> 5, "hi" -> 2)

  m.get("hello") shouldBe Option(5)
  m.get("world") shouldBe None
  m.withPrefix("h") shouldBe PrefixMap("ello" -> 5, "i" -> 2)

  val m2 = m += "foo" -> 3
  m2.get("foo") shouldBe Option(3)

}
