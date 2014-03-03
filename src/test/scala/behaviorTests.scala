package edu.luc.cs.cs372.team2.shapeAlgebraic

import org.scalatest.FunSuite
import scalaz.syntax.equal._
import scalaz.std.anyVal._ // for assert_=== to work on Int


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
  testBoundingBox("simple group", simpleGroup cata boundingBox, 150, 70, 350, 280)
  testBoundingBox("complex group", complexGroup cata boundingBox, 30, 60, 470, 320)

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
}
