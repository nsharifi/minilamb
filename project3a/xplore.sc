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
