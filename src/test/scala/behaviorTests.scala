package edu.luc.cs.cs372.team2.shapeAlgebraic

import edu.luc.cs.cs372.team2.shapeAlgebraic.structures.Ellipse
import org.scalatest.FunSuite
import scalaz.syntax.equal._
import scalaz.std.anyVal._ // for assert_=== to work on Int
import fixtures._


class behaviorTests extends FunSuite {

  import scalamu.ToMuOps
  import scalamu.In
  import behaviors._
  import structures._

  def testBoundingBox(desc: String, l: Location[Rectangle], x: Int, y: Int, width: Int, height: Int) {
    test(desc) {
      val r = l.shape
      assert(x == l.x)
      assert(y == l.y)
      assert(width == r.width)
      assert(height == r.height)
    }
  }
  import fixtures._
  testBoundingBox("simple ellipse works", simpleEllipse cata boundingBox, -50, -30, 100, 60)
  testBoundingBox("simple rectangle", simpleRectangle cata boundingBox, 0, 0, 80, 120)
  testBoundingBox("simple location", simpleLocation cata boundingBox, 70, 30, 80, 120)
  testBoundingBox("basic group", basicGroup cata boundingBox, -50, -30, 100, 70)
  testBoundingBox("simple group", simpleGroup cata boundingBox, 150, 50, 350, 300)
  testBoundingBox("complex group", complexGroup cata boundingBox, 30, 80, 470, 300)

  test("size works") {
    simpleEllipse cata size assert_=== 1
    simpleLocation cata size assert_=== 1
    basicGroup cata size assert_=== 2
    simpleGroup cata size assert_=== 2
    complexGroup cata size assert_=== 5
  }

  test("depth works") {
    simpleEllipse cata depth assert_=== 1
    simpleLocation cata depth assert_=== 2
    basicGroup cata depth assert_=== 2
    simpleGroup cata depth assert_=== 3
    complexGroup cata depth assert_=== 6
  }

  test("scale works") {
    //fixtures.simpleRectangle cata scale(1)  assert_=== Rectangle(80,120)
  }
  /* to set up test for scale */
  def testScale1(description: String, factor: Int) = {

    val expectedShape = Ellipse(150,90)
    //val actualShape = scale(simpleEllipse, factor).asInstanceOf[Ellipse]
    //assert(actualShape.a === expectedShape.a)
    //assert(actualShape.halfHeight === expectedShape.halfHeight)
  }
}
