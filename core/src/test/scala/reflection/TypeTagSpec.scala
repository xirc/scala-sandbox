package reflection

import testing.BaseSpec

import scala.reflect.runtime.universe._

final class TypeTagSpec extends BaseSpec {

  def paramInfo[T: TypeTag](x: T): Option[List[Type]] = {
    typeOf[T] match {
      case TypeRef(_, _, args) => Option(args)
      case _                   => None
    }
  }

  paramInfo(1) shouldBe Some(List.empty)
  paramInfo(List(1, 2, 3)) shouldBe Option(List(definitions.IntTpe))

  def weakParamInfo[T: WeakTypeTag](x: T): Option[List[Type]] = {
    weakTypeOf[T] match {
      case TypeRef(_, _, args) => Option(args)
      case _                   => None
    }
  }

  weakParamInfo(1) shouldBe Some(List.empty)
  weakParamInfo(List(1, 2, 3)) shouldBe Some(List(definitions.IntTpe))

  def typeParamsViaTypeTag[T: TypeTag]: Option[List[Type]] =
    paramInfo(List.empty[T])
  def typeParamsViaWeakTypeTag[T]: Option[List[Type]] =
    weakParamInfo(List.empty[T])

  typeParamsViaTypeTag[Int] shouldBe Some(
    List(definitions.IntTpe)
  )
  typeParamsViaWeakTypeTag[Int] should not be Some(
    List(definitions.IntTpe)
  )

}
