package macros

import scala.collection.mutable
import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object PrintfMacroExample {

  def printf(format: String, params: Any*): Unit =
    macro printf_impl

  def printf_impl(
      c: blackbox.Context
  )(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
    import c.universe._

    val fmt: String = format.tree match {
      case Literal(Constant(fmt: String)) =>
        fmt
      case _ =>
        throw new UnsupportedOperationException(
          "printf should be string literal"
        )
    }

    var valdefs: Seq[ValDef] = Vector.empty
    def precompute(valueTree: Tree, tpe: Type): Ident = {
      val termName = TermName(c.freshName())
      valdefs :+=
        ValDef(Modifiers(), termName, TypeTree(tpe), valueTree)
      Ident(termName)
    }

    val prints: Array[Tree] = {
      val paramStack: mutable.Stack[Tree] =
        mutable.Stack.from(params.view.map(_.tree))
      val refs: Array[Tree] = fmt.split("(?<=%[\\w%])|(?=%[\\w%])").map {
        case "%d"  => precompute(paramStack.pop(), typeOf[Int])
        case "%f"  => precompute(paramStack.pop(), typeOf[Double])
        case "%s"  => precompute(paramStack.pop(), typeOf[String])
        case "%%"  => Literal(Constant("%"))
        case other => Literal(Constant(other))
      }
      refs.map(ref =>
        reify {
          print(c.Expr[Any](ref).splice)
        }.tree
      )
    }

    val stats: Seq[Tree] = valdefs ++ prints
    c.Expr[Unit](q"..$stats")

  }

}
