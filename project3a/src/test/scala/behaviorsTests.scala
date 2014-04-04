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

  val const = In(Constant(3))
  val var_ = In(Var("x"))
  val if_ = In(If(In(Constant(0)), In(Constant(3)), In(Constant(4))))
  val fun0 = In(Fun("x", In(Plus(In(Constant(7)), In(Var("x"))))))
  val fun1 = In(App(In(Fun("x", In(Plus(In(Constant(7)), In(Var("x")))))), In(Constant(3))))
  //println (fun1)
  test("eval works") {
    const cata(eval) assert_=== In(Constant(3))
    var_ cata eval assert_=== In(Var("x"))
    if_ cata eval assert_=== In(Constant(4))
    fun0 cata eval assert_=== fun0

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