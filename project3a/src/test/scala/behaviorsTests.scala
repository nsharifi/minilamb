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
  /*A*/
  val a1 = In(Constant(3))
  val a2 = In(Var("x"))
  val a3 = In(Fun("x", In(Plus(In(Constant(7)), In(Var("x"))))))
  val a4 = In(App(In(Fun("x", In(Plus(In(Constant(7)), In(Var("x")))))), In(Constant(3))))/*error*/
  //val a5=....

  /*B*/
  val b1 = In(If(In(Constant(7)), In(Constant(3)), In(Constant(4))))
  val b2 = In(If(In(Constant(0)), In(Constant(3)), In(Constant(4))))
  //val b3 = In(If( In(Fun("x",In(Var("x")), In(Constant(3)), In(Constant(4))))))
  //val b4 = In(If( In(Fun("x",In(Var("y")), In(Constant(3)), In(Constant(4))))))

  /*C*/
  //val c1 = In(App.....



  test("eval works") {
    a1 cata eval assert_=== In(Constant(3))
    a2 cata eval assert_=== In(Var("x"))
    a3 cata eval assert_=== fixtures.a3
    //a4...

    b1 cata eval assert_=== In(Constant(3))
    b2 cata eval assert_=== In(Constant(4))
    //b3 cata eval assert_=== In(Constant(3))
    //b4 cata eval assert_=== In(Constant(3))

    //c1....

    //fun1 cata eval assert_=== In(Constant(10))
    //val min3 = evaluate(UMinus(evaluate(Constant(3))))
    //In(Plus(In(Constant(7)), In(Constant(3)))) cata eval assert_=== In(Constant(10))
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