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
  #A
  1. eval(Const(3)) -> Const(3)
  2. eval(Var("x")) -> error
  3.eval(Fun("x", Plus(Const(7), Var("x")))) -> Fun("x", Plus(Const(7), Var("x")))
  4. eval(App(Fun("x", Plus(Const(7), Var("x"))), Const(3))) -> Const(10)
  eval(App(Var("x"), Const(3)) -> error
  #B
  1.eval(If(Const(7), Const(3), Const(4))) -> Const(3)
  2.eval(If(Const(0), Const(3), Const(4))) -> Const(4)
  3.eval(If(Fun("x", Var("x")), Const(3), Const(4))) -> Const(3)
  4.eval(If(Fun("x", Var("y")), Const(3), Const(4))) -> Const(3)

  #C
  1.val Y = // you'll define it yourself as in the handout!
    2.eval(App(App(Y, Fun("f", Fun("n", If(Var("n"),
      Times(Var("n"), App(Var("f"), Minus(Var("n"), Const(1)))), Const(1))))),
      Const(5)))
  -> Const(120)
*/
  import structures.ExprFactory._

  test("interpret works") {
    assert(interpret(plus(constant(2), constant(3))) == constant(5))
    assert(interpret(constant(3)) == In(Constant(3)))
    assert(interpret(variable("x")) == In(Error("Variable")))
    assert(interpret(fun("x", plus(constant(7), variable("x")))) == In(Fun("x", In(Plus(In(Constant(7)), In(Var("x")))))))
    assert(interpret(app(variable("x"), constant(3))) == In(Error("Var Application")))
    //assert(interpret(app(fun("x", plus(constant(7), variable("x"))), constant(3))) == In(Constant(10)))
    //B
    assert(interpret( iff(constant(7), constant(3), constant(4)) ) == In(Constant(3)))
    assert(interpret(iff(constant(0), constant(3), constant(4)) ) == In(Constant(4)))
    assert(interpret(iff(variable("x"), constant(3), constant(4)) ) == In(Error("Var Conditional")))
   // assert(interpret(iff(fun("x", variable("x")), constant(3), constant(4)) ) == In(Constant(4)))
    assert(interpret(fun("x", plus(constant(7), variable("x")))) == In(Fun("x", In(Plus(In(Constant(7)), In(Var("x")))))))
    assert(interpret(app(variable("x"), constant(3))) == In(Error("Var Application")))
    assert(interpret(app(fun("x", plus(constant(7), variable("x"))), constant(3))) == In(Constant(10)))
  }


//  test("evaluate works") {
//    fixtures.complex1 cata evaluate assert_=== -1
//    fixtures.complex2 cata evaluate assert_=== 0
//  }
//
  test("size works") {
    fixtures.complex1 cata size assert_=== 9
    fixtures.complex2 cata size assert_=== 10
  }

  test("depth works") {
    fixtures.complex1 cata depth assert_=== 4
    fixtures.complex2 cata depth assert_=== 5
  }
}