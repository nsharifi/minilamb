package project3a

import scalamu.In

object behaviors {

  import scalamu.Algebra
  import structures._

  //In(App(In(Fun("x", In(Plus(In(Const(7)), In(Var("x")))))), In(Const(3)))) -> Const(10)
  // substitute a for x in e
  def reduce(e: Expr, a: Expr, x: Expr): Expr = e match {
    case In(Var(_)) => e match {
      case x => a
      case _ => e
    }
    case In(Constant(c)) => In(Constant(c))
    case In(Fun(v, b)) => v match {
      case x => In(Fun(v, b))
      case _ => {
        In(Fun("y1", reduce(reduce(e, In(Var("y1")), In(Var(v))), a, x)))
      }
    }
  }

  def interpret(expr: Expr): Expr = expr match {
    case In(Constant(c)) => In(Constant(c))
    case In(UMinus(In(Constant(r))))   => In(Constant(-r))
    case In(Plus(In(Constant(l)), In(Constant(r))))  => In(Constant(l + r))
    case In(Minus(In(Constant(l)), In(Constant(r)))) => In(Constant(l - r))
    case In(Times(In(Constant(l)), In(Constant(r)))) => In(Constant(l * r))
    case In(Div(In(Constant(l)), In(Constant(r))))   => In(Constant(l / r))
    case In(Mod(In(Constant(l)), In(Constant(r))))   => In(Constant(l % r))
    case In(Var(n))      => In(Error("Variable"))
    case In(If(c, t, e)) => (c,t,e) match {
      case (In(Constant(x)), _, _) => x match {  /* Case constant check lhs, rhs*/
        case 0 => interpret(e)/*rhs*/
        case _ => interpret(t)/*lhs*/
      }
      case (In(Var(x)), _, _) => In(Error("Var Conditional"))
      case (_, _, _) => interpret(In(If(c, t, e)))
    }
    case In(Fun(v, b))   => In(Fun(v, b))
    case In(App(l, r))   => (l, r) match {
      case (In(Var(_)), _) => In(Error("Var Application"))
      case (In(Fun(v, b)), x) => interpret(reduce(b, x, In(Var(v))))
    }
  }

//  def evaluate: Algebra[ExprF, Int] = {
//    case Constant(c) => c
//    case UMinus(Constant(r))   => In(Constant(-r))
//    case Plus(Constant(l), Constant(r))  => l + r
//    case Minus(Constant(l), Constant(r)) => l - r
//    case Times(Constant(l), Constant(r)) => l * r
//    case Div(Constant(l), Constant(r))   => l / r
//    case Mod(Constant(l), Constant(r))   => l % r
//  }
//
//
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