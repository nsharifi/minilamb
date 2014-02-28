package edu.luc.cs.cs372.team2.shapeAlgebraic

import org.scalatest.FunSuite
import scalaz.syntax.equal._
import scalaz.std.anyVal._ // for assert_=== to work on Int

class behaviorTests extends FunSuite {

  import scalamu.ToMuOps
  import behaviors._

  test("size works") { fixtures.Ellipse cata size assert_=== 1  }
  /*  If you add Equal and Show instances for your ShapeF and Shape, you will be able to use
  assert_===   like in "equality works" here        */


}
