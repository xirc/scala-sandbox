package collection.custom.rna

private sealed trait Base
private object Base {
  case object A extends Base
  case object U extends Base
  case object G extends Base
  case object C extends Base

  val fromInt: Int => Base = Array(A, U, G, C)
  val toInt: Base => Int = Map(A -> 0, U -> 1, G -> 2, C -> 3)
}
