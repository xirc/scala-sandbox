package ch6

import shapeless._
import testing.BaseSpec

final class SimpleOpsExample extends BaseSpec {

  ("abc" :: 123 :: true :: HNil).last shouldBe true
  ("abc" :: 123 :: true :: HNil).init shouldBe ("abc" :: 123 :: HNil)

}
