package project3a

import scalamu.In

object behaviors {

  import scalamu.Algebra
  import structures._

  //In(App(In(Fun("x", In(Plus(In(Const(7)), In(Var("x")))))), In(Const(3)))) -> Const(10)
  def betaSub(v: Expr, b: Expr): Expr = (v, b) match {
    case (_, _) => In(Constant(10))
    //case _ => ???
  }
  //App(Fun("x", Plus(Const(7), Var("x"))), Const(3)) -> Const(10)

  def interpret(expr: Expr): Expr = expr match {
    case In(Constant(c)) => In(Constant(c))
    case In(UMinus(r))   => In(UMinus(r))
    case In(Plus(l, r))  => In(Plus(l, r))
    case In(Minus(l, r)) => In(Minus(l, r))
    case In(Times(l, r)) => In(Times(l, r))
    case In(Div(l, r))   => In(Div(l, r))
    case In(Mod(l, r))   => In(Mod(l, r))
    case In(Var(n))      => In(Error("Variable"))
    case In(If(c, t, e)) => (c,t,e) match {
      case (In(Constant(x)), _, _) => x match {
        case 0 => interpret(e)
        case _ => interpret(t)
      }
      case (_, _, _) => interpret(In(If(c, t, e)))
    }
    case In(Fun(v, b))   => In(Fun(v, b))
    case In(App(l, r))   => (l, r) match {
      case (In(Var(_)), _) => In(Error("Var Application"))
      //case (In(Fun(v, b)), _) =>
      //case In(Plus(l, r)) => ???
    }
  }

  val evaluate: Algebra[ExprF, Int] = {
    case Constant(c) => c
    case UMinus(r)   => -r
    case Plus(l, r)  => l + r
    case Minus(l, r) => l - r
    case Times(l, r) => l * r
    case Div(l, r)   => l / r
    case Mod(l, r)   => l % r
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