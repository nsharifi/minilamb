package project3a

import scalamu.In

object behaviors {

  import structures._
  import structures.ExprFactory._


  // Variable generating code
  var counter = 0
  def nextVar: String = {
    counter += 1
    "y" + counter
  }

  // substitute a for x in e
  def reduce(e: Expr, x: String, a: Expr): Expr = e match {
    case In(Var(_)) => e match {
      case x => a
      case _ => e
    }
    case In(Constant(c)) => constant(c)
    case In(Var(v)) => if(e == x) a else e
    case In(UMinus(r)) => uminus(a) // TODO recursion on a?
    case In(Plus(l, r)) => plus(reduce(l, x, a), reduce(r, x, a))
    case In(Minus(l, r)) => minus(reduce(l, x, a), reduce(r, x, a))
    case In(Times(l, r)) => times(reduce(l, x, a), reduce(r, x, a))
    case In(Mod(l, r)) => mod(reduce(l, x, a), reduce(r, x, a))
    case In(Div(l, r)) => div(reduce(l, x, a), reduce(r, x, a))
    case In(If(cond, then, elze)) => iff(reduce(cond, x, a), reduce(then, x, a), reduce(elze, x, a))

    case In(Fun(y, b)) => {
      if(y == x) fun(y, b)
      else {
        val newVar = nextVar
        fun(newVar, reduce(b, y, variable(newVar))) // ???
      }
    }
    case In(App(l, r)) => app(reduce(l, x, a), reduce(r, x, a))
  }

  def eval(expr: Expr): Expr = expr match {
    case In(Constant(c))                             => constant(c)
    case In(UMinus(r)) => r match {
      case In(Constant(r)) => constant(-r)
      case _ => uminus(eval(r))
    }
    case In(Plus(l, r))  => (l, r) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l + r)
    }
    case In(Minus(l, r)) => (l, r) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l - r)
      case (_, _) => minus(eval(l), eval(r))
    }
    case In(Times(l, r)) => (l,r) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l * r)
      case (_, _) => times(eval(l), eval(r))
    }
    case In(Div(l, r))   => (l, r) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l / r)
      case (_, _) => div(eval(l), eval(r))
    }
    case In(Mod(l, r))   => (l, r) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l % r)
      case (_, _) => mod(eval(l), eval(r))
    }
    case In(Var(v)) => err("Var")
    case In(If(c, t, e)) => (c,t,e) match {
      case (In(Constant(x)), _, _) => x match {  /* Case constant check lhs, rhs*/
        case 0 => eval(e)/*rhs*/
        case _ => eval(t)/*lhs*/
      }
      case (In(Var(x)), _, _) => err("Var Conditional")
      case (In(Fun(_, _)), _, _) => eval(t) // TODO check this
      case _ => iff(eval(c), eval(t), eval(e))
    }
    case In(Fun(v, b))   => fun(v, b)

    case In(App(l, r))   => eval(l) match {
      case In(Fun(v1, b1)) => eval(reduce(b1, v1, r))
      case _ => err("Application of Non-Function")
    }
  }

}