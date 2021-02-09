package testing

import org.scalatest.Inside
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

abstract class BaseSpec
    extends AnyWordSpecLike
    with Matchers
    with ScalaFutures
    with Inside
