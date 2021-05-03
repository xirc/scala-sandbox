package ch4

import cats.data.State
import cats.syntax.all._
import testing.BaseSpec

final class CatsStateSpec extends BaseSpec {

  type CalcState[A] = State[List[Int], A]

  def evalOne(sym: String): CalcState[Int] = {
    def operator(f: (Int, Int) => Int): CalcState[Int] = State {
      case b :: a :: tail =>
        val c = f(a, b)
        (c :: tail, c)
      case _ =>
        sys.error("fail")
    }
    def operand(value: Int): CalcState[Int] = State { stack =>
      (value :: stack, value)
    }
    sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }
  }

  evalOne("42").runA(Nil).value shouldBe 42

  val program = for {
    _ <- evalOne("1")
    _ <- evalOne("2")
    ans <- evalOne("+")
  } yield ans

  program.runA(Nil).value shouldBe 3

  def evalAll(input: List[String]): CalcState[Int] = {
    input.foldLeft(0.pure[CalcState]) { (acc, value) =>
      acc.flatMap(_ => evalOne(value))
    }
  }

  // ((1 + 2) * 3 - 1) / 4
  val multistageProgram = evalAll(
    List("1", "2", "+", "3", "*", "1", "-", "4", "/")
  )
  multistageProgram.runA(Nil).value shouldBe 2

  def evalInput(input: String): Int = {
    evalAll(input.split(" ").toList).runA(Nil).value
  }

  evalInput("3 3 + 2 2 * - 2 /") shouldBe 1

}
