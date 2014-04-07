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
  def reduce(e: Expr, a: Expr, x: Expr): Expr = e match {
    case In(Var(_)) => e match {
      case x => a
      case _ => e
    }
    case In(Constant(c)) => constant(c)
    case In(UMinus(r)) => uminus(a)
    case In(Plus(l, r)) if(r == x) => plus(l, a)
    case In(Plus(l, r)) if(l == x) => plus(a, r)
    case In(Minus(l, r)) if(r == x) => plus(l, a)
    case In(Minus(l, r)) if(l == x) => plus(a, r)
    case In(Times(l, r)) if(r == x) => plus(l, a)
    case In(Times(l, r)) if(l == x) => plus(a, r)
    case In(Mod(l, r)) if(r == x) => plus(l, a)
    case In(Mod(l, r)) if(l == x) => plus(a, r)
    case In(Div(l, r)) if(r == x) => plus(l, a)
    case In(Div(l, r)) if(l == x) => plus(a, r)

    case In(Fun(y, b)) if (y == x) => fun(y, b)
    case In(Fun(y, b)) if (y != x) => {/* Iterator for bound variable re-naming. if y!=x, x free in e, y free in a, new y'*/
      val curVar = nextVar
      fun(variable(curVar), reduce(reduce(b, variable(curVar), y), a, x))
    }
  }

  def eval(expr: Expr): Expr = expr match {
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
        case 0 => eval(e)/*rhs*/
        case _ => eval(t)/*lhs*/
      }
      case (In(Var(x)), _, _) => err("Var Conditional")
      case (In(Fun(_, _)), _, _) => eval(t)
    }
    case In(Fun(v, b))   => fun(v, b)

    case In(App(l, r))   => (l, r) match {
      case (In(Fun(v, b)), x) => eval(reduce(b, x, v))
      case (_, _) => err("Application of Non-Function")
    }
  }

}