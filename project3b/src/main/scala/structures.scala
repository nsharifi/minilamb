package project3a
import scalaz.{ Equal, Functor, Show }
import scalamu._ // algebra types and injected cata method

/*
 * In this example, we represent arithmetic expressions as trees
 * (initial algebra for the endofunctor defined next).
 */

object structures {

  /**
   * Endofunctor for (nongeneric) F-algebra in the category Scala types.
   * Note that `A` is not a generic item type of the resulting algebraic
   * data type. As can be seen below, once we form `Expr` as the least
   * fixpoint of `ExprF`, `A` will go away.
   *
   * @tparam A argument of the endofunctor
   */
  sealed trait ExprF[+A]
  case class Constant(value: Int) extends ExprF[Nothing]
  case class UMinus[A](expr: A) extends ExprF[A]
  case class Plus[A](left: A, right: A) extends ExprF[A]
  case class Minus[A](left: A, right: A) extends ExprF[A]
  case class Times[A](left: A, right: A) extends ExprF[A]
  case class Div[A](left: A, right: A) extends ExprF[A]
  case class Mod[A](left: A, right: A) extends ExprF[A]
  case class Var(name: String) extends ExprF[Nothing]
  case class Fun[A](v: String, body: A) extends ExprF[A]
  case class App[A](left: A, right: A) extends ExprF[A]
  case class Iff[A](cond: A, t_hen: A, elze: A) extends ExprF[A]
  case class Error(e: String) extends ExprF[Nothing]
  //3b
  case class Cell[A](left: A, right: A) extends ExprF[A]
  case class Hd[A](expr: A) extends ExprF[A]
  case class Tl[A](expr: A) extends ExprF[A]

  /**
   * Implicit value for declaring `ExprF` as an instance of
   * typeclass `Functor` in scalaz. This requires us to define
   * `map`.
   */
  implicit object ExprFFunctor extends Functor[ExprF] {
    def map[A, B](fa: ExprF[A])(f: A => B): ExprF[B] = fa match {
      case Constant(v) => Constant(v)
      case UMinus(r)   => UMinus(f(r))
      case Plus(l, r)  => Plus(f(l), f(r))
      case Minus(l, r) => Minus(f(l), f(r))
      case Times(l, r) => Times(f(l), f(r))
      case Div(l, r)   => Div (f(l), f(r))
      case Mod(l, r)   => Mod (f(l), f(r))
      case Var(n)      => Var(n)
      case Fun(v, b)   => Fun(v, f(b))
      case App(l, r)   => App(f(l), f(r))
      case Iff(c, t, e) => Iff(f(c), f(t), f(e))
      case Error(e)    => Error(e)

      //3b
      case Cell(l, r)   => Cell(f(l), f(r))
      case Hd(r)   => Hd(f(r))
      case Tl(r)   => Tl(f(r))
    }
  }

  /**
   * Implicit value for declaring `ExprF` as an instance of
   * typeclass `Equal` in scalaz using `Equal`'s structural equality.
   * This enables `===` and `assert_===` on `ExprF` instances.
   */
  //implicit def ExprFEqual[A]: Equal[ExprF[A]] = Equal.equalA

  /**
   * Implicit value for declaring `ExprF` as an instance of
   * typeclass `Show` in scalaz using `Show`'s default method.
   * This is required for `===` and `assert_===` to work on `ExprF` instances.
   */
  //implicit def ExprFShow[A]: Show[ExprF[A]] = Show.showFromToString

  /**
   * Least fixpoint of `ExprF` as carrier object for the initial algebra.
   */
  type Expr = µ[ExprF]

  /**
   * Factory for creating Expr instances.
   */
  object ExprFactory {
    def constant(c: Int): Expr = In(Constant(c))
    def uminus(r: Expr): Expr = In(UMinus(r))
    def plus(l: Expr, r: Expr): Expr = In(Plus (l, r))
    def minus(l: Expr, r: Expr): Expr = In(Minus(l, r))
    def times(l: Expr, r: Expr): Expr = In(Times(l, r))
    def div(l: Expr, r: Expr): Expr = In(Div (l, r))
    def mod(l: Expr, r: Expr): Expr = In(Mod (l, r))
    def variable(n: String): Expr = In(Var(n))
    def fun(v: String, b: Expr) = In(Fun(v, b))
    def app(l: Expr, r: Expr) = In(App(l, r))
    def iff(c: Expr, t: Expr, e: Expr) = In(Iff(c, t, e))
    def err(e: String) = In(Error(e))

    //3B
    def cell(l: Expr, r: Expr) = In(Cell(l, r))
    def hd(r: Expr): Expr = In(Hd(r))
    def tl(r: Expr): Expr = In(Tl(r))

  }
}