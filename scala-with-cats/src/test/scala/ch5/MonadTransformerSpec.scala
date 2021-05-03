package ch5

import cats.data.EitherT
import testing.BaseSpec

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future}

final class MonadTransformerSpec extends BaseSpec {

  type Response[A] = EitherT[Future, String, A]

  val powerLevels = Map(
    "Jazz" -> 6,
    "Bumblebee" -> 8,
    "Hot Rod" -> 10
  )

  def getPowerLevel(autobot: String): Response[Int] = {
    powerLevels.get(autobot) match {
      case Some(power) =>
        EitherT.right(Future(power))
      case None =>
        EitherT.left(Future(s"$autobot unreachable"))
    }
  }

  def canSpecialMove(ally1: String, ally2: String): Response[Boolean] = {
    val SpecialAttackPowerLevel = 16
    for {
      power1 <- getPowerLevel(ally1)
      power2 <- getPowerLevel(ally2)
    } yield {
      (power1 + power2) >= SpecialAttackPowerLevel
    }
  }

  def tacticalReport(ally1: String, ally2: String): String = {
    Await.result(canSpecialMove(ally1, ally2).value, 3.seconds) match {
      case Left(cause) =>
        s"Error: $cause"
      case Right(true) =>
        s"$ally1 and $ally2 are ready to roll out!"
      case Right(false) =>
        s"$ally1 and $ally2 need a recharge."
    }
  }

  tacticalReport(
    "Jazz",
    "Bumblebee"
  ) shouldBe "Jazz and Bumblebee need a recharge."

  tacticalReport(
    "Bumblebee",
    "Hot Rod"
  ) shouldBe "Bumblebee and Hot Rod are ready to roll out!"

  tacticalReport(
    "Jazz",
    "Ironhide"
  ) shouldBe "Error: Ironhide unreachable"

}
