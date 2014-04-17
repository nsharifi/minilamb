package project3a

import org.scalatest.FunSuite

class behaviorTests extends FunSuite {

  import behaviors._
  import structures.ExprFactory._


  test("reduce works") {
    assert(reduce(variable("x"), "x", plus(constant(3), constant(4))) == plus(constant(3), constant(4)))
    assert(reduce(variable("y"), "x", plus(constant(1), constant(2))) == variable("y"))
    assert(reduce(variable("y"), "x", fun("z", plus(constant(1), constant(2)))) == variable("y"))
    assert(reduce(constant(10), "x", plus(constant(1), constant(2))) == constant(10))
    assert(reduce(fun("y", constant(3)), "y", constant(5)) == fun("y", constant(3)))
    assert(reduce(plus(variable("x"), constant(2)), "x", constant(3)) == plus(constant(3), constant(2)))
    assert(reduce(fun("y", variable("y")), "x", constant(5)) == fun("y1", variable("y1")))
    assert (reduce(fun("y", plus(variable("y"), plus(app(fun("x", variable("x")), constant(3)), variable("x")))), "x", constant(5)) ==
      fun("y2", plus(variable("y2"), plus(app(fun("y4", variable("y4")), constant(3)), constant(5))) ))
  }


  test("eval part A works") {
    assert(eval(constant(3)) == constant(3))
    assert(eval(variable("x")) == err("Variable"))
    assert(eval(fun("x", plus(constant(7), variable("x")))) == fun("x", plus(constant(7), variable("x"))))
    assert(eval(app(variable("x"), constant(3))) == err("Application of Non-Function"))
    assert(eval(app(fun("x", plus(constant(7), variable("x"))), constant(3))) == constant(10))
  }

  test("eval part B works") {
    assert(eval( iff(constant(7), constant(3), constant(4)) ) == constant(3))
    assert(eval(iff(constant(0), constant(3), constant(4)) ) == constant(4))
    assert(eval(iff(variable("x"), constant(3), constant(4)) ) == err("Conditional Error"))
    assert(eval(iff(fun("x", variable("x")), constant(3), constant(4))) == constant(3))
    assert(eval(iff(fun("x", variable("y")), constant(3), constant(4))) == constant(3))
  }/*pass*/

  test("eval part C  works") {
    assert(eval(fun("x", plus(constant(7), variable("x")))) == fun("x", plus(constant(7), variable("x"))))
    assert(eval(app(variable("x"), constant(3))) == err("Application of Non-Function"))
  }/*pass*/

  test("eval Y works") {
    assert(eval(app(app(fixtures.Y, fun("f", fun("n", iff(variable("n"),
      times(variable("n"), app(variable("f"), minus(variable("n"), constant(1)))), constant(1))))),
      constant(5))) == constant(120))
  }

  //
  test("variable generator works") {assert (nextVar != nextVar) }/* pass!!Two calls to nextVar generate different values*/
////====================================================
//  //3b
//  //#A
//  //  eval(Cell(Plus(Const 3, Const 7), Minus(Const 5, Const 2))) -> Cell(Plus(Const 3, Const 7), Minus(Const 5, Const 2))
////    eval(Hd(Cell(Plus(Const 3, Const 7), Minus(Const 5, Const 2)))) -> Const 10
//  //  eval(Tl(Cell(Plus(Const 3, Const 7), Minus(Const 5, Const 2)))) -> Const 3
//  //#B
//  //  eval(If(Cell(...), Const 3, Const 4)) -> Const 3
//  //  eval(Hd(Const 0)) -> error
//  //    eval(Tl(Const 0)) -> error
//  //    eval(Tl(Fun("x", Var("x")))) -> error
//  //  eval(Tl(Cell(Const 10, Fun("x", Var("x"))))) -> Fun("x", Var("x"))
//
  test("eval part Cell A works") {
        assert(eval( cell(plus(constant(3), constant(7)), minus(constant(5),constant(2))) ) ==
          cell(plus(constant(3), constant(7)), minus(constant(5),constant(2))))
    assert(eval( hd(cell(plus(constant(3), constant(7)), minus(constant(5),constant(2))) )) ==  constant(10))
    assert(eval( tl(cell(plus(constant(3), constant(7)), minus(constant(5),constant(2))) )) ==  constant(3))

  }/*pass*/
  test("eval part Cell B works") {
    //eval(If(Cell(...), Const 3, Const 4)) -> Const 3
    assert(eval( hd(constant(0))) ==  err("Non-cell Head"))
    assert(eval( tl(constant(0))) ==  err("Non-cell Tail"))
    assert(eval(tl(fun("x", variable("x")))) == err("Non-cell Tail"))
    assert( eval(tl(cell(constant(10), fun("x", variable("x"))))) == fun("x", variable("x")))

  }/*pass*/

  //cell(10, cell(20, cell(30, 0))) has size and length 3
//  test("eval part preLength1 works") {
//    assert(eval(app(app(fixtures.Y,(fun("f", fun("c", iff(variable("c"), plus(constant(1),
//      app(variable("f"), tl(variable("c")))), constant(0))))))
//      ,cell(constant(0),cell(constant(20),cell(constant(30),constant(0)))))) == constant(3))
// }

  //cell(cell(10, 11), cell(cell(20, 21), cell(cell(30,31), 0))) //TODO preLength and preSize test for this




}/*behaviourTest*/