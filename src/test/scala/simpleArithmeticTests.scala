import org.scalatest.FunSuite
import project3c.{fixtures, structures, behaviors}

class simpleArithmeticTests  extends FunSuite {

  import behaviors._
  import structures.ExprFactory._
  import fixtures._

  test("eval arithmetic works") {

    assert(eval(twoPlusthree) == five)
    assert(eval(threeMinusone) == two)
    assert(eval(threeTimestwo) == six)
  }

}/*simpleArithmetic*/
