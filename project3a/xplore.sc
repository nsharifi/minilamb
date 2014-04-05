import scalaz.Functor

println("Team 2/9")

sealed trait ExprF[+A]
case class Var_X[A](x:A) extends ExprF[A]
case class Fun_X[A](left: A, right: A) extends ExprF[A]
case class If_X[A](left: A, right: A) extends ExprF[A]
case class App_X[A](left: A, right: A) extends ExprF[A]

implicit object ExprFFunctor extends Functor[ExprF] {

  def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
  case Var_X(x) => Var_X(f(x))
  case Fun_X(l,r)=>Fun_X(f(l),f(r))

}
}

import scalamu.Algebra
val eval: Algebra[ExprF, Int] = {

  case Var_X(x) => x
  case _ => -1 // if left side not equal to ?? zero

}

// Testing Either
val in = "abc"
val res: Either[String, Int] = try {
  Right(in.toInt)
} catch  {
  case e: Exception =>
    Left(in)
}
res match {
  case Right(_) => println ("Success: "+res.right.get)
  case Left(_) => println ("Failed: "+res.left.get)
}

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
  //val one = Lambda(Var("s"), Var("s"), Lambda(Var("s"), Lambda(Var("z"), Var("z")))
  println(eval(Var("xx")))
}