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
    case In(Constant(c)) => constant(c)
    case In(Var(`x`)) => a
    case In(Var(_)) => e
    case In(UMinus(r)) => eval(uminus(a))
    case In(Plus(l, r)) => plus(reduce(l, x, a), reduce(r, x, a))
    case In(Minus(l, r)) => minus(reduce(l, x, a), reduce(r, x, a))
    case In(Times(l, r)) => times(reduce(l, x, a), reduce(r, x, a))
    case In(Mod(l, r)) => mod(reduce(l, x, a), reduce(r, x, a))
    case In(Div(l, r)) => div(reduce(l, x, a), reduce(r, x, a))
    case In(Iff(cond, t_hen, elze)) => iff(reduce(cond, x, a), reduce(t_hen, x, a), reduce(elze, x, a))

    case In(Fun(`x`, b)) => fun(x, b)
    case In(Fun(y, b)) => {
      val newVar = nextVar
      // Do α-reduction
      val alphaReduced = reduce(b, y, variable(newVar))
      // Do β-reduction
      fun(newVar, reduce(alphaReduced, x, a))
    }
    case In(App(l, r)) => app(reduce(l, x, a), reduce(r, x, a))
  }/* reduce*/

  def eval(expr: Expr): Expr = expr match {
    case In(Constant(c))  => constant(c)
    case In(UMinus(l)) => eval(l) match {
      case In(Constant(l)) => constant(-l)
    }
    case In(Plus(l, r)) => (eval(l), eval(r)) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l + r)
    }
    case In(Minus(l, r)) => (eval(l), eval(r)) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l - r)
    }
    case In(Times(l, r)) => (eval(l), eval(r)) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l * r)
    }
    case In(Div(l, r)) => (eval(l), eval(r)) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l / r)
    }
    case In(Mod(l, r)) => (eval(l), eval(r)) match {
      case (In(Constant(l)), In(Constant(r))) => constant(l % r)
    }

    case In(Var(v)) => err("Var")

    case In(Iff(c, t, e)) => (c,t,e) match {
      case (In(Constant(x)), _, _) => x match {  /* Case constant check lhs, rhs*/
        case 0 => eval(e)/*rhs*/
        case _ => eval(t)/*lhs*/
      }
      case (In(Var(x)), _, _) => err("Var Conditional")
      case (In(Fun(_, _)), _, _) => eval(t) // TODO 4 check this
      case _ => iff(eval(c), eval(t), eval(e))
    }/*Iff*/

    case In(Fun(v, b))   => fun(v, b)

    case In(App(l, r))   => eval(l) match {
      case In(Fun(v1, b1)) => eval(reduce(b1, v1, r))
      case _ => err("Application of Non-Function")
    }
    //3b
    case In(Cell(l, r))  => (l, r) match{
      case (h, In(Nill())) => eval(h) // TODO Alternative to Nill implementation???
      case (_, _) => cell(l, r)
//        In(Cell(e,a1)) //e11 TODO 5 - NOT SURE
    }
    case In(Hd(e)) => e match {
      case In(Cell(e11, a11)) => eval(e11)
      case _ => err("Non-cell Head")
    }
    case In(Tl(e)) => e match {
      case In(Cell(e11,a)) => eval(a)
      case _ => err("Non-cell Tail")
    }

  }/*eval*/

}