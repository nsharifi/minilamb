///import _root_.struct.Expr

/**
 * Created by naser on 4/2/14.
 */
object struct {
  sealed trait Expr
  case class Var(name: String) extends Expr
  case class Lambda(v: Var, body: Expr) extends Expr
  case class App(left: Expr, right: Expr) extends Expr

  def eval(expr: Expr) = expr match {
    case Var(name) => s"Variable: $name"
    case App(left, right) => "Apply " + left + " to " + right
    case Lambda(v, body) => "Lambda $v " + " . " + body
  }
}

object test extends App {
  import struct._
  val id: Lambda = Lambda(Var("x"), Var("x"))
  val zero = Lambda(Var("s"), Var("z"))
  val one = Lambda(Var("s"), Var("s")), Lambda(Var("s"), Lambda(Var("z"), Var("z")))
  println(eval(Var("xx")))
}