package concurrent

import org.scalatest.time.{Seconds, Span}
import testing.BaseSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.util.Try

final class PromiseSpec extends BaseSpec {
  override implicit def patienceConfig: PatienceConfig = PatienceConfig(
    scaled(Span(3, Seconds))
  )

  private def defer[T](
      func: => T,
      delay: FiniteDuration = 200.millis
  ): Future[T] = {
    Future {
      Thread.sleep(delay.toMillis)
      func
    }
  }

  "success" in {

    val promise = Promise[Int]()
    val future = promise.future
    defer {
      promise.success(2)
    }
    future.futureValue shouldBe 2

    a[IllegalStateException] shouldBe thrownBy {
      promise.success(3)
    }

    a[IllegalStateException] shouldBe thrownBy {
      promise.failure(new RuntimeException())
    }

  }

  "failure" in {

    val promise = Promise[Int]()
    val future = promise.future
    defer {
      promise.failure(new RuntimeException())
    }
    future.failed.futureValue shouldBe a[RuntimeException]

    a[IllegalStateException] shouldBe thrownBy {
      promise.success(3)
    }

    a[IllegalStateException] shouldBe thrownBy {
      promise.failure(new RuntimeException())
    }

  }

  "complete" in {

    val successPromise = Promise[Int]()
    val successFuture = successPromise.future
    defer {
      successPromise.complete(Try(1))
    }
    successFuture.futureValue shouldBe 1

    val failurePromise = Promise[Int]()
    val failureFuture = failurePromise.future
    defer {
      failurePromise.complete(Try {
        throw new RuntimeException()
      })
    }
    failureFuture.failed.futureValue shouldBe a[RuntimeException]

  }

}
