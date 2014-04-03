package project3a

import scalamu.In

object behaviors {

  import scalamu.Algebra
  import structures._

//  def betaSub(v: Expr, b: Expr): Expr = {
//    case _ => ???
//  }
def scale: Algebra[ExprF, Expr] = {
  case Constant(c)   => In(Constant(c))
}

  val eval: Algebra[ExprF, Expr] = {
    case Constant(c) => In(Constant(c))
    case UMinus(r)   => In(UMinus(r))
    case Plus(l, r)  => In(Plus(l, r))
    case Minus(l, r) => In(Minus(l, r))
    case Times(l, r) => In(Times(l, r))
    case Div(l, r)   => In(Div(l, r))
    case Mod(l, r)   => In(Mod(l, r))
    case Var(n)      => In(Var(n))
    case If(c, t, e) => (c,t,e) match {
      case (In(Constant(0)), _, _) => e
      case (_, _, _) => t
    }
    case Fun(v, b)   => In(Fun(v, b))
//    case App(l, r)   => ??? //eval(Fun(l, r))
    case _ => ???
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