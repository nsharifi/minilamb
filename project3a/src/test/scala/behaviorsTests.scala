package project3a

import org.scalatest.FunSuite
import scalaz.syntax.equal._
import scalaz.std.anyVal._ // for assert_=== to work on Int

class behaviorTests extends FunSuite {

  import scalamu.ToMuOps
  import scalamu.In
  import behaviors._
  import structures._

  /* Test cases to be built on these
  eval(Const(3)) -> Const(3)
  eval(Var("x")) -> error
  eval(Fun("x", Plus(Const(7), Var("x")))) -> Fun("x", Plus(Const(7), Var("x")))
  eval(App(Fun("x", Plus(Const(7), Var("x"))), Const(3))) -> Const(10)
  eval(App(Var("x"), Const(3)) -> error
  eval(If(Const(7), Const(3), Const(4))) -> Const(3)
  eval(If(Const(0), Const(3), Const(4))) -> Const(4)
  eval(If(Fun("x", Var("x")), Const(3), Const(4))) -> Const(3)
  eval(If(Fun("x", Var("y")), Const(3), Const(4))) -> Const(3)
  val Y = // you'll define it yourself as in the handout!
  eval(App(App(Y, Fun("f", Fun("n", If(Var("n"),
      Times(Var("n"), App(Var("f"), Minus(Var("n"), Const(1)))), Const(1))))),
      Const(5)))
  -> Const(120)
*/
import structures.ExprFactory._

  test("interpret works") {
    assert(interpret(constant(3)) == In(Constant(3)))
    assert(interpret(variable("x")) == In(Error("Variable")))
    assert(interpret( iff(constant(7), constant(3), constant(4)) ) == In(Constant(3)))
    assert(interpret(iff(constant(0), constant(3), constant(4)) ) == In(Constant(4)))

  }

  test("evaluate works") {
    fixtures.complex1 cata evaluate assert_=== -1
    fixtures.complex2 cata evaluate assert_=== 0
  }

  test("size works") {
    fixtures.complex1 cata size assert_=== 9
    fixtures.complex2 cata size assert_=== 10
  }

  test("depth works") {
    fixtures.complex1 cata depth assert_=== 4
    fixtures.complex2 cata depth assert_=== 5
  }
}