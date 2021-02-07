package pattern.magnet

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration

object Main extends App {
  import DSL._

  implicit val timeout: Duration = Duration.Inf

  println(complete(Future(1)))
  println(complete(List(Future(1), Future(2))))
  println(complete(List(1, 2, 4, 3, 0)))
  println(complete(1, 2))
  println(complete())
  println(complete {
    print("!")
    "?"
  })

}
