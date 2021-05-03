package testing

import org.scalatest.{EitherValues, TryValues}
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpecLike

import scala.concurrent.duration.DurationInt

abstract class BaseSpec
    extends AnyWordSpecLike
    with Matchers
    with ScalaFutures
    with TryValues
    with EitherValues {

  override implicit def patienceConfig: PatienceConfig = {
    PatienceConfig(scaled(3.seconds))
  }

}
