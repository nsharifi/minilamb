package project3a

import scalamu.In

object behaviors {

  import scalamu.Algebra
  import structures._
  import structures.ExprFactory._

  //In(App(In(Fun("x", In(Plus(In(Const(7)), In(Var("x")))))), In(Const(3)))) -> Const(10)
  // substitute a for x in e
  def reduce(e: Expr, a: Expr, x: Expr): Expr = e match {
    case In(Var(_)) => e match {
      case x => a
      case _ => e
    }
    case In(Constant(c)) => constant(c)
    case In(Plus(l, r)) => plus(l, r)
    case In(Fun(v, b)) => v match {
      case x => fun(v, b)
      case _ => {
        fun(variable("y1"), reduce(reduce(e, variable("y1"), v), a, x))
      }
    }
  }

  def interpret(expr: Expr): Expr = expr match {
    case In(Constant(c))                             => constant(c)
    case In(UMinus(In(Constant(r))))                 => constant(-r)
    case In(Plus(In(Constant(l)), In(Constant(r))))  => constant(l + r)
    case In(Minus(In(Constant(l)), In(Constant(r)))) => constant(l - r)
    case In(Times(In(Constant(l)), In(Constant(r)))) => constant(l * r)
    case In(Div(In(Constant(l)), In(Constant(r))))   => constant(l / r)
    case In(Mod(In(Constant(l)), In(Constant(r))))   => constant(l % r)
    case In(Var(n))                                  => err("Variable")
    case In(If(c, t, e)) => (c,t,e) match {
      case (In(Constant(x)), _, _) => x match {  /* Case constant check lhs, rhs*/
        case 0 => interpret(e)/*rhs*/
        case _ => interpret(t)/*lhs*/
      }
      case (In(Var(x)), _, _) => err("Var Conditional")
      case (_, _, _) => interpret(In(If(c, t, e)))
    }
    case In(Fun(v, b))   => fun(v, b)
    case In(App(l, r))   => (l, r) match {
      case (In(Var(_)), _) => err("Var Application")
      case (In(Fun(v, b)), x) => interpret(reduce(b, x, v))
    }
  }

  val size: Algebra[ExprF, Int] = {
    case Constant(_) => 1
    case UMinus(r)   => 1 + r
    case Plus(l, r)  => 1 + l + r
    case Minus(l, r) => 1 + l + r
    case Times(l, r) => 1 + l + r
    case Div(l, r)   => 1 + l + r
    case Mod(l, r)   => 1 + l + r
  }

  val depth: Algebra[ExprF, Int] = {
    case Constant(_) => 1
    case UMinus(r)   => 1 + r
    case Plus(l, r)  => 1 + math.max(l, r)
    case Minus(l, r) => 1 + math.max(l, r)
    case Times(l, r) => 1 + math.max(l, r)
    case Div(l, r)   => 1 + math.max(l, r)
    case Mod(l, r)   => 1 + math.max(l, r)
  }
}