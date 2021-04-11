package pattern.magnet

import testing.BaseSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.concurrent.duration.Duration

final class CompletionSpec extends BaseSpec {
  import DSL._
  private implicit val timeout: Duration = Duration.Inf

  "complete works well" in {
    complete(Future(1)) shouldBe 1
    complete(List(Future(1), Future(2))) shouldBe List(1, 2)
    complete(List(1, 2, 4, 3, 0)) shouldBe List(0, 1, 2, 3, 4)
    complete { s"1" } shouldBe "11"
    // Could I write better than below?
    complete((1, 2)) shouldBe 1
    complete(()) shouldBe ()
  }

}
