package edu.luc.cs.cs372.team2.shapeAlgebraic

import edu.luc.cs.cs372.team2.shapeAlgebraic.structures.Ellipse
import org.scalatest.FunSuite
import scalaz.syntax.equal._
import scalaz.std.anyVal._ // for assert_=== to work on Int
import fixtures._

class behaviorTests extends FunSuite {

  import scalamu.ToMuOps
  import behaviors._

  test("boundingBox works") {
    //fixtures.simpleEllipse cata boundingBox assert_===Location(-50,30,Rectangle(100,60))
    //fixtures.basicGroup cata boundingBox assert_=== 0
  }

  test("size works") {
    fixtures.simpleEllipse cata size assert_=== 1
    fixtures.simpleLocation cata size assert_=== 1
    fixtures.basicGroup cata size assert_=== 2
    fixtures.simpleGroup cata size assert_=== 2
    fixtures.complexGroup cata size assert_=== 5
  }

  test("depth works") {
    fixtures.simpleEllipse cata depth assert_=== 1
    fixtures.simpleLocation cata depth assert_=== 2
    fixtures.basicGroup cata depth assert_=== 2
    fixtures.simpleGroup cata depth assert_=== 3
    fixtures.complexGroup cata depth assert_=== 6
  }
  /* to set up test for scale */
  def testScale1(description: String, factor: Int) = {

    val expectedShape = Ellipse(150,90)
    //val actualShape = scale(simpleEllipse, factor).asInstanceOf[Ellipse]
    //assert(actualShape.a === expectedShape.a)
    //assert(actualShape.halfHeight === expectedShape.halfHeight)
  }
}
