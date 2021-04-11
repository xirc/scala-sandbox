package pattern.magnet

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.implicitConversions

trait CompletionMagnet {
  type Result
  def apply(): Result
}
object CompletionMagnet {

  implicit def fromFuture[T](
      future: Future[T]
  )(implicit timeout: Duration): CompletionMagnet =
    new CompletionMagnet {
      override type Result = T
      override def apply(): T = Await.result(future, timeout)
    }

  implicit def fromFutureList[T](
      future: List[Future[T]]
  )(implicit
      executionContext: ExecutionContext,
      timeout: Duration
  ): CompletionMagnet =
    new CompletionMagnet {
      override type Result = List[T]
      override def apply(): List[T] =
        Await.result(Future.sequence(future), timeout)
    }

  implicit def fromSortableList[T: Ordering](xs: List[T]): CompletionMagnet =
    new CompletionMagnet {
      override type Result = List[T]
      override def apply(): List[T] = xs.sorted
    }

  implicit def fromTuple[A, B](ts: (A, B)): CompletionMagnet =
    new CompletionMagnet {
      override type Result = A
      override def apply(): A = ts._1
    }

  implicit def fromByName(s: => String): CompletionMagnet =
    new CompletionMagnet {
      override type Result = String
      override def apply(): String = s + s
    }

  implicit def fromUnit(u: Unit): CompletionMagnet =
    new CompletionMagnet {
      override type Result = Unit
      override def apply(): Unit = ()
    }

}
