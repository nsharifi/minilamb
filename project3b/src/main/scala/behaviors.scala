package project3a

import scalamu.In

object behaviors {

  import structures._
  import structures.ExprFactory._



  var counter = 0
  def nextVar: String = {/*Variable generating code*/
    counter += 1
    "y" + counter
  }

  // substitute x with a in e
  def reduce(e: Expr, x: String, a: Expr): Expr = e match {
    case In(Constant(c)) => constant(c)
    case In(Var(`x`))    => a
    case In(Var(_))      => e
    case In(UMinus(r))   => eval(uminus(a))
    case In(Plus(l, r))  => plus(reduce(l, x, a), reduce(r, x, a))
    case In(Minus(l, r)) => minus(reduce(l, x, a), reduce(r, x, a))
    case In(Times(l, r)) => times(reduce(l, x, a), reduce(r, x, a))
    case In(Mod(l, r))   => mod(reduce(l, x, a), reduce(r, x, a))
    case In(Div(l, r))   => div(reduce(l, x, a), reduce(r, x, a))
    case In(Iff(cond, t_hen, e_lse)) => iff(reduce(cond, x, a), reduce(t_hen, x, a), reduce(e_lse, x, a))

    case In(Fun(`x`, b)) => fun(x, b)
    case In(Fun(y, b))   => {
      val newVar = nextVar
      // α-reduction
      val alphaReduced = reduce(b, y, variable(newVar))
      // β-reduction
      fun(newVar, reduce(alphaReduced, x, a))
    }
    case In(App(l, r))   => app(reduce(l, x, a), reduce(r, x, a))
    case In(Hd(c)) => hd(reduce(c, x, a))
    case In(Tl(c)) => tl(reduce(c, x, a))
    case In(IsCell(c)) => iscell(reduce(c, x, a))
    case _               => { err("Unpredicted case") }

  }/* reduce*/

  def eval(expr: Expr): Expr = expr match {
    case In(Constant(c))   => constant(c)
    case In(UMinus(v))     => eval(v) match {
      case In(Constant(l)) => constant(-l)
    }
    case In(Plus(l, r))    => (eval(l), eval(r)) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l + r)
      case _                              => plus(eval(l), eval(r))
    }
    case In(Minus(l, r))   => (eval(l), eval(r)) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l - r)
      case In(_)                              => minus(eval(l), eval(r))
    }
    case In(Times(l, r))   => (eval(l), eval(r)) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l * r)
      case In(_)                              => times(eval(l), eval(r))
    }
    case In(Div(l, r))     => (eval(l), eval(r)) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l / r)
      case In(_)                              => div(eval(l), eval(r))
    }
    case In(Mod(l, r))     => (eval(l), eval(r)) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l % r)
      case In(_)                              => mod(eval(l), eval(r))
    }

    case In(Iff(c, t, e))  => eval(c) match {
      case In(Error(_))    => err("Conditional Error")
      case In(Constant(0)) => eval(e)
      case _               => eval(t)
    }
    case In(Var(_)) => err("Variable")

    case In(Fun(v, b))     => fun(v, b)

    case In(App(l, r))     => eval(l) match {
      case In(Fun(v1, b1)) => eval(reduce(b1, v1, r))
      case In(Var(_)) => eval(r)
      case _ => err("Application of Non-Function")
    }
    //3b
    case In(Cell(l, r))    => cell(l, r)
    case In(Hd(c))         => eval(c) match {
      case In(Cell(h, t)) => eval(h)
      case _                  => err("Non-cell Head")
    }
    case In(Tl(c))        => eval(c) match {
      case In(Cell(h, t)) => eval(t)
      case _                => err("Non-cell Tail")
    }
    case In(IsCell(c))   => eval(c) match {
      case In(Cell(_, _))  => constant(1)
      case _               => constant(0)
    }
  }

}
