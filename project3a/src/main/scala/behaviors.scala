package project3a

object behaviors {

  import scalamu.Algebra
  import structures._

  def betaSub(expr: Expr): Expr = expr match {
    case _ => ???
  }

  val eval: Algebra[ExprF, Expr] = {
    case Constant(c) => c
    case UMinus(r)   => -r
    case Plus(l, r)  => l + r
    case Minus(l, r) => l - r
    case Times(l, r) => l * r
    case Div(l, r)   => l / r
    case Mod(l, r)   => l % r
    case Var(n)      => n
    case Fun(v, b) => eval(betaSub(b))
    case App(l, r)   => eval(Fun(l, r))

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