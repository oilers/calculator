package calculator

sealed abstract class Expr
final case class Literal(v: Double) extends Expr
final case class Ref(name: String) extends Expr
final case class Plus(a: Expr, b: Expr) extends Expr
final case class Minus(a: Expr, b: Expr) extends Expr
final case class Times(a: Expr, b: Expr) extends Expr
final case class Divide(a: Expr, b: Expr) extends Expr

object Calculator {
  def computeValues(
      namedExpressions: Map[String, Signal[Expr]]): Map[String, Signal[Double]] = {
    namedExpressions.map({case (name, expr) => (name, Signal(eval(expr(), namedExpressions)))})
  }

  def eval(expr: Expr, references: Map[String, Signal[Expr]]): Double = {
    def evalRec(expr: Expr, references: Map[String, Signal[Expr]], stack: List[Expr]): Double = {
      lazy val newStack = expr::stack
      if(stack contains expr) Double.NaN
      else expr match {
        case Literal(x) => x
        case Ref(name) => evalRec(getReferenceExpr(name, references), references, newStack)
        case Plus(a, b) => evalRec(a, references, newStack) + evalRec(b, references, newStack)
        case Minus(a, b) => evalRec(a, references, newStack) - evalRec(b, references, newStack)
        case Times(a, b) => evalRec(a, references, newStack) * evalRec(b, references, newStack)
        case Divide(a, b) => evalRec(a, references, newStack) / evalRec(b, references, newStack)
      }
    }
    evalRec(expr, references, List())
  }

  /** Get the Expr for a referenced variables.
   *  If the variable is not known, returns a literal NaN.
   */
  private def getReferenceExpr(name: String,
      references: Map[String, Signal[Expr]]) = {
    references.get(name).fold[Expr] {
      Literal(Double.NaN)
    } { exprSignal =>
      exprSignal()
    }
  }
}
