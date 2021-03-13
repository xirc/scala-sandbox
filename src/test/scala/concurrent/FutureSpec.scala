package concurrent

import testing.BaseSpec

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import scala.concurrent.{Await, Future, Promise}
import scala.util.{Failure, Success}

final class FutureSpec extends BaseSpec {

  private def future[T](
      value: => T,
      delay: FiniteDuration = 200.millis
  ): Future[T] = Future {
    Thread.sleep(scaled(delay).toMillis)
    value
  }

  "Await.result" in {

    val success = future(1)
    Await.result(success, 3.seconds) shouldBe 1

    val failure = future { throw new RuntimeException() }
    a[RuntimeException] shouldBe thrownBy {
      Await.result(failure, 3.seconds)
    }

  }

  "onComplete" in {

    val success = future(2)
    success.onComplete {
      case Success(value) => value shouldBe 2
      case Failure(cause) => fail(cause)
    }
    Await.ready(success, 3.seconds)

    val failure = future[Int] { throw new RuntimeException() }
    failure.onComplete {
      case Success(value)     => fail(value.toString)
      case Failure(exception) => exception shouldBe a[RuntimeException]
    }
    Await.ready(failure, 3.seconds)

  }

  "map" in {

    val success = future(1).map(_ * 2).map(_ * 3).map(_ * 4)
    success.futureValue shouldBe 24

    val failure = future(1).map(_ * 2).map(_ / 0).map(_ * 5)
    failure.failed.futureValue shouldBe a[ArithmeticException]

  }

  "flatMap" in {

    val success = future(1)
      .flatMap { value => future(value * 2) }
      .flatMap { value => future(value + 10) }
    success.futureValue shouldBe 12

    val failure = future(1)
      .flatMap { _ => future[Int] { throw new RuntimeException() } }
      .flatMap { value => future(value * 2) }
    failure.failed.futureValue shouldBe a[RuntimeException]

  }

  "for comprehension" in {

    val success = for {
      v1 <- future(1)
      v2 <- future(v1 * 2)
      v3 <- future(v2 + 10)
    } yield {
      v3
    }
    success.futureValue shouldBe 12

    val failure = for {
      _ <- future(1)
      v2 <- future[Int] { throw new RuntimeException() }
      v3 <- future(v2 * 2)
    } yield {
      v3
    }
    failure.failed.futureValue shouldBe a[RuntimeException]

  }

  "sequence" in {

    val successFutures = (1 to 5).map(i => {
      future(i * 2)
    })
    val success = Future.sequence(successFutures)
    success.futureValue shouldBe Seq(2, 4, 6, 8, 10)

    val futuresContainingFailure = (4 to 0 by -1).map(i => {
      future(100 / i)
    })
    val failure = Future.sequence(futuresContainingFailure)
    failure.failed.futureValue shouldBe a[ArithmeticException]

  }

  "traverse" in {

    val values = Seq(4, 3, 2, 1, 0)

    val success = Future.traverse(values)(x => future { x * 2 })
    success.futureValue shouldBe Seq(8, 6, 4, 2, 0)

    val failure = Future.traverse(values)(x => future { 100 / x })
    failure.failed.futureValue shouldBe a[ArithmeticException]

  }

  "foldLeft" in {

    val success = Future.foldLeft((1 to 10).map(future(_)))(0)(_ + _)
    success.futureValue shouldBe 55

    val failure =
      Future.foldLeft((10 to 0 by -1).map(x => future { 1 / x }))(0)(_ + _)
    failure.failed.futureValue shouldBe a[ArithmeticException]

  }

  "reduceLeft" in {

    val success = Future.reduceLeft((1 to 10).map(future(_)))(_ + _)
    success.futureValue shouldBe 55

    val failure =
      Future.reduceLeft((10 to 0 by -1).map(x => future { 1 / x }))(_ + _)
    failure.failed.futureValue shouldBe a[ArithmeticException]

  }

  "firstCompletedOf" should {

    "success" in {
      val promises = Seq.tabulate(2)(_ => Promise[Int]())
      val success = Future.firstCompletedOf(promises.map(_.future))
      promises.head.success(1)
      success.futureValue shouldBe 1

      promises.last.success(2)
      Await.ready(promises.last.future, 1.seconds)
      success.futureValue shouldBe 1
    }

    "failure" in {
      val promises = Seq.tabulate(2)(_ => Promise[Int]())
      val failure = Future.firstCompletedOf(promises.map(_.future))
      promises.last.failure(new RuntimeException())
      failure.failed.futureValue shouldBe a[RuntimeException]

      promises.head.success(1)
      Await.ready(promises.head.future, 1.seconds)
      failure.failed.futureValue shouldBe a[RuntimeException]
    }

    "timeout" in {
      val neverCompleted = Future.firstCompletedOf(Vector.empty[Future[Int]])
      a[TimeoutException] shouldBe thrownBy {
        Await.result(neverCompleted, 1.seconds)
      }
    }

  }

  "zip" in {
    val success1 = future { 1 }
    val success2 = future { 2 }
    val failure = future[Int] { throw new RuntimeException() }

    success1.zip(success2).futureValue shouldBe ((1, 2))
    success1.zip(failure).failed.futureValue shouldBe a[RuntimeException]

  }

  "zipWith" in {

    val success1 = future { 1 }
    val success2 = future { 2 }
    val failure = future[Int] { throw new RuntimeException() }

    success1.zipWith(success2)(_ + _).futureValue shouldBe 3
    success1
      .zipWith(failure)(_ + _)
      .failed
      .futureValue shouldBe a[RuntimeException]

  }

  "andThen" in {
    val successCount = new AtomicInteger(0)
    val failureCount = new AtomicInteger(0)

    val success = future { 1 }
      .andThen { case Success(_) =>
        successCount.incrementAndGet()
      }

    success.futureValue shouldBe 1
    successCount.get() shouldBe 1
    failureCount.get() shouldBe 0

    val failure = future { throw new RuntimeException() }
      .andThen { case Failure(_) =>
        failureCount.incrementAndGet()
      }

    failure.failed.futureValue shouldBe a[RuntimeException]
    successCount.get() shouldBe 1
    failureCount.get() shouldBe 1

  }

  "recover" in {

    val recovered = future { 0 }
      .map(1 / _)
      .recover { case _: ArithmeticException =>
        0
      }
    recovered.futureValue shouldBe 0

    val failure = future[Int] { throw new RuntimeException() }
      .recover { case _: ArithmeticException =>
        0
      }
    failure.failed.futureValue shouldBe a[RuntimeException]

  }

  "recoverWith" in {

    val recovered = future { 0 }
      .map(1 / _)
      .recoverWith { case _: ArithmeticException =>
        future { 0 }
      }
    recovered.futureValue shouldBe 0

    val failure = future[Int] { throw new RuntimeException() }
      .recoverWith { case _: ArithmeticException =>
        future { 0 }
      }
    failure.failed.futureValue shouldBe a[RuntimeException]

  }

  "fallbackTo" in {

    val success = future { 0 }
      .map(1 / _)
      .fallbackTo(future { 0 })
    success.futureValue shouldBe 0

    val failure = future { 0 }
      .map(1 / _)
      .fallbackTo(future { throw new RuntimeException() })
    failure.failed.futureValue shouldBe a[RuntimeException]

  }

}
