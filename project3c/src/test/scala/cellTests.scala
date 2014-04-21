import org.scalatest.FunSuite
import project3a.{fixtures, structures, behaviors}

class cellTests extends FunSuite {

  import behaviors._
  import structures.ExprFactory._
  import fixtures._

  test("eval part Cell A works") {
    assert(eval( cell(plus(constant(3), constant(7)), minus(constant(5),constant(2))) ) ==
      cell(plus(constant(3), constant(7)), minus(constant(5),constant(2))))
    assert(eval( hd(cell(plus(constant(3), constant(7)), minus(constant(5),constant(2))) )) ==  constant(10))
    assert(eval( tl(cell(plus(constant(3), constant(7)), minus(constant(5),constant(2))) )) ==  constant(3))

  }/*pass*/
  test("eval part Cell B works") {
    assert(eval(iff(cell(plus(constant(3), constant(7)), minus(constant(5),constant(2))), constant(3), constant(4))) == constant(3))
    assert(eval(iff(cell(constant(10), cell(constant(7), cell(constant(5), constant(0)))), constant(5), constant(0))) == constant(5))
    assert(eval( hd(constant(0))) ==  err("Non-cell Head"))
    assert(eval( tl(constant(0))) ==  err("Non-cell Tail"))
    assert(eval(tl(fun("x", variable("x")))) == err("Non-cell Tail"))
    assert( eval(tl(cell(constant(10), fun("x", variable("x"))))) == fun("x", variable("x")))
    assert( eval(tl(cellComplex1)) == cell(constant(20), cell(constant(30), constant(0))))

  }/*pass*/
  val preAdd = fun("f", fun("n", iff(variable("n"),
      plus(variable("n"), app(variable("f"), hd(variable("n")))), constant(0))))
  test("preAdd") {
//    assert (iff(cell(constant(5), constant(0)), plus()))
    assert (eval(app(app(Y, preAdd), cell(constant(10), cell(constant(20), constant(0))))) == constant(30))
  }
  test("eval part preLength works") {
//    assert(eval(app(app(Y, preLength), cell(constant(1), constant(0)))) == constant(1))
     assert(eval(app(app(Y, preLength), cellComplex1)) == three)
//     assert(eval(app(app(Y, preLength),cellComplex2)) == three)
  }

  test("eval part preSize works") {
//     assert(eval(app(app(Y, preSize),cellComplex1)) == 3)
//     assert(eval(app(app(Y, preSize),cellComplex2)) == 6)
  }

}/*endcellTests*/

/*
  3b
  #A
    eval(Cell(Plus(Const 3, Const 7), Minus(Const 5, Const 2))) -> Cell(Plus(Const 3, Const 7), Minus(Const 5, Const 2))
    eval(Hd(Cell(Plus(Const 3, Const 7), Minus(Const 5, Const 2)))) -> Const 10
    eval(Tl(Cell(Plus(Const 3, Const 7), Minus(Const 5, Const 2)))) -> Const 3
  #B
    eval(If(Cell(...), Const 3, Const 4)) -> Const 3
    eval(Hd(Const 0)) -> error
      eval(Tl(Const 0)) -> error
      eval(Tl(Fun("x", Var("x")))) -> error
    eval(Tl(Cell(Const 10, Fun("x", Var("x"))))) -> Fun("x", Var("x"))

    */
