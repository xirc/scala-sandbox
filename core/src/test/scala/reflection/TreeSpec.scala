package reflection

import testing.BaseSpec

import scala.reflect.runtime.universe
import scala.reflect.runtime.universe._

final class TreeSpec extends BaseSpec {

  "show" in {

    val x: Int = 1
    val expr: universe.Expr[Int] = reify {
      x + 2
    }
    show(expr.tree) shouldBe "x.$plus(2)"

  }

  "traversal via pattern match" in {

    val x: Int = 1
    val expr: universe.Expr[Int] = reify {
      x + 2
    }

    val (f, arg) = expr.tree match {
      case Apply(f, arg :: Nil) => (f, arg)
      case _                    => fail()
    }
    show(f) shouldBe "x.$plus"
    show(arg) shouldBe "2"

  }

  "traversal via pattern match (2)" in {

    val x = 1
    val expr: universe.Expr[Int] = reify {
      x + 2 + 3
    }

    val (f, arg) = expr.tree match {
      case Apply(f, arg :: Nil) => (f, arg)
      case _                    => fail()
    }
    show(f) shouldBe "x.$plus(2).$plus"
    show(arg) shouldBe "3"

  }

  "traversal via traverser" in {

    val x = 1
    val expr: universe.Expr[Int] = reify {
      x + 2 + 3
    }

    object traverser extends Traverser {
      var applies: List[Apply] = List.empty
      override def traverse(tree: universe.Tree): Unit = tree match {
        case apply @ Apply(f, args) =>
          applies = apply :: applies
          super.traverse(f)
          super.traverseTrees(args)
        case _ =>
          super.traverse(tree)
      }
    }

    traverser.traverse(expr.tree)

    traverser.applies.size shouldBe 2

    inside(traverser.applies(0)) { app =>
      show(app.fun) shouldBe "x.$plus"
      show(app.args.head) shouldBe "2"
    }

    inside(traverser.applies(1)) { app =>
      show(app.fun) shouldBe "x.$plus(2).$plus"
      show(app.args.head) shouldBe "3"
    }

  }

  "splice" in {

    val x = reify(2)
    val y = reify(x.splice + 3)
    show(y.tree) shouldBe "2.$plus(3)"

  }

}
