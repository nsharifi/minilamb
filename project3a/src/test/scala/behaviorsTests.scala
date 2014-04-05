package project3a

import org.scalatest.FunSuite

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
    assert(interpret(constant(3)) == constant(3))
    assert(interpret(variable("x")) == err("Variable"))
    assert(interpret(fun(variable("x"), plus(constant(7), variable("x")))) ==
      fun(variable("x"), plus(constant(7), variable("x"))))
    assert(interpret(app(variable("x"), constant(3))) == err("Var Application"))
    assert(interpret(app(fun(variable("x"), plus(constant(7), variable("x"))), constant(3))) == constant(10))
    //B
    assert(interpret( iff(constant(7), constant(3), constant(4)) ) == constant(3))
    assert(interpret(iff(constant(0), constant(3), constant(4)) ) == constant(4))
    assert(interpret(iff(variable("x"), constant(3), constant(4)) ) == err("Var Conditional"))
    assert(interpret(iff(fun(variable("x"), variable("x")), constant(3), constant(4))) == constant(3))
    assert(interpret(iff(fun(variable("x"), variable("y")), constant(3), constant(4))) == constant(3))
    assert(interpret(fun(variable("x"), plus(constant(7), variable("x")))) ==
      fun(variable("x"), plus(constant(7), variable("x"))))
    assert(interpret(app(variable("x"), constant(3))) == err("Var Application"))
  }
  test("reduce works") {
    assert (reduce(variable("x"), plus(constant(3), constant(4)), variable("x")) == plus(constant(3), constant(4)))
    assert (reduce(variable("y"), plus(constant(1), constant(2)), variable("x")) == plus(constant(1), constant(2)))
    assert (reduce(variable("y"), fun(variable("z"), plus(constant(1), constant(2))), variable("x")) ==
      fun(variable("z"), plus(constant(1), constant(2))))
    assert (reduce(constant(10), plus(constant(1), constant(2)), variable("x")) == constant(10))
    assert (reduce(fun(variable("y"), constant(3)), constant(5), variable("y")) == fun(variable("y"), constant(3)))
    assert (reduce(fun(variable("y"), constant(3)), constant(5), variable("x")) != fun(variable("y"), constant(3)))

//println(reduce(fun(variable("y"), constant(3)), constant(5), variable("x")))
  }
}