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
    complete(1, 2) shouldBe 1
    complete() shouldBe ()
  }

  "complete(: => T) should use carefully" in {
    var call_count = 0
    complete {
      call_count += 1
      s"${call_count},"
    } shouldBe "1,1,"
  }

}
