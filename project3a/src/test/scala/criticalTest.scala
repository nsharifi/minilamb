import org.scalatest.FunSuite
import project3a.{structures, behaviors}

class criticalTest extends FunSuite {

  import behaviors._
  import structures.ExprFactory._


  test("Alpha substitution") {
    assert(reduce(variable("x"), "x", variable("y")) == variable("y"))
    assert(reduce(variable("x"), "y", variable("z")) == variable("x"))
    assert( reduce(fun("x", plus(variable("x"), variable("y"))), "y", variable("z")) ==
      fun("y1", plus(variable("y1"), variable("z"))))
    assert (reduce(fun("y", app(variable("x"), fun("x", plus(variable("x"), variable("y"))))), "x", variable("y")) ==
      fun("y1", app(variable("y"), fun("x", plus(variable("x"), variable("y1"))))))
//    assert (reduce(fun("x", fun("y", app(variable("x"), fun("x", plus(variable("x"), variable("y")))))), "x", variable("y")) ==
//      fun("y1", app(variable("y"), fun("x", plus(variable("x"), variable("y1"))))))
  }



}

