package project3a

import org.scalatest.FunSuite

class behaviorTests extends FunSuite {

  import behaviors._
  import structures.ExprFactory._

  test("misc tests") {
//    assert (eval(plus(variable("x"))))
  }

  test("reduce works") {
    assert(reduce(variable("x"), "x", plus(constant(3), constant(4))) == plus(constant(3), constant(4)))
    assert(reduce(variable("y"), "x", plus(constant(1), constant(2))) == plus(constant(1), constant(2)))
    assert(reduce(variable("y"), "x", fun("z", plus(constant(1), constant(2)))) ==
      fun("z", plus(constant(1), constant(2))))
    assert(reduce(constant(10), "x", plus(constant(1), constant(2))) == constant(10))
    assert(reduce(fun("y", constant(3)), "y", constant(5)) == fun("y", constant(3)))
//    assert(reduce(fun(variable("y"), constant(3)), variable("x"), constant(5)) == fun(variable("y"), constant(3)))
    assert(reduce(plus(variable("x"), constant(2)), "x", constant(3)) == plus(constant(3), constant(2)))

//    assert (reduce(fun(variable("x"), plus(variable("x"), constant(5))), variable("x"), constant(5)) == plus(constant(5), constant(5)))
  }

  test("eval arithmetic works") {
    assert(eval(fixtures.twoPlusthree) == constant(5))
    assert(eval(fixtures.threeMinusone) == constant(2))
    assert(eval(fixtures.threeTimestwo) == constant(6))
  }

  test("eval part A works") {
    assert(eval(constant(3)) == constant(3))
//    assert(eval(variable("x")) == err("Variable"))
    assert(eval(fun("x", plus(constant(7), variable("x")))) == fun("x", plus(constant(7), variable("x"))))
    assert(eval(app(variable("x"), constant(3))) == err("Application of Non-Function"))
    assert(eval(app(fun("x", plus(constant(7), variable("x"))), constant(3))) == constant(10))
  }

  test("eval part B works") {
    assert(eval( iff(constant(7), constant(3), constant(4)) ) == constant(3))
    assert(eval(iff(constant(0), constant(3), constant(4)) ) == constant(4))
    assert(eval(iff(variable("x"), constant(3), constant(4)) ) == err("Var Conditional"))
    assert(eval(iff(fun("x", variable("x")), constant(3), constant(4))) == constant(3))
    assert(eval(iff(fun("x", variable("y")), constant(3), constant(4))) == constant(3))
  }

  test("eval part C  works") {
    assert(eval(fun("x", plus(constant(7), variable("x")))) == fun("x", plus(constant(7), variable("x"))))
    assert(eval(app(variable("x"), constant(3))) == err("Application of Non-Function"))
  }



  test("eval Y works") {
//    assert(eval(app(app(fixtures.Y, fun("f", fun("n", iff(variable("n"),
//      times(variable("n"), app(variable("f"), minus(variable("n"), constant(1)))), constant(1))))),
//      constant(5))) == constant(120)) //err("Application of Non-Function"))
  }

  test("variable generator works") {assert (nextVar != nextVar) }/* Two calls to nextVar generate different values*/
//====================================================
  //project3b
  //3b
  //#A
  //  eval(Cell(Plus(Const 3, Const 7), Minus(Const 5, Const 2))) -> Cell(Plus(Const 3, Const 7), Minus(Const 5, Const 2))
  //  eval(Hd(Cell(Plus(Const 3, Const 7), Minus(Const 5, Const 2)))) -> Const 10
  //  eval(Tl(Cell(Plus(Const 3, Const 7), Minus(Const 5, Const 2)))) -> Const 3
  //#B
  //  eval(If(Cell(...), Const 3, Const 4)) -> Const 3
  //  eval(Hd(Const 0)) -> error
  //    eval(Tl(Const 0)) -> error
  //    eval(Tl(Fun("x", Var("x")))) -> error
  //  eval(Tl(Cell(Const 10, Fun("x", Var("x"))))) -> Fun("x", Var("x"))

  test("eval part Cell A works") {
    //    assert(eval( cell(plus(constant(3), constant(7)), minus(constant(5),constant(2))) ) ==
    //      cell(plus(constant(3), constant(7)), minus(constant(5),constant(2))))
  }
}