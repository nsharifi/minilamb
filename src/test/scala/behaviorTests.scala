package edu.luc.cs.cs372.team2.shapeAlgebraic

import org.scalatest.FunSuite
import scalaz.syntax.equal._
import scalaz.std.anyVal._ // for assert_=== to work on Int

class behaviorTests extends FunSuite {

  import scalamu.ToMuOps
  //import scalamu._
  import behaviors._
  import structures._

  test("boundingBox works") {
    fixtures.simpleEllipse cata boundingBox assert_=== (Location(-50, -30, Rectangle(100, 60)))
    fixtures.basicGroup cata boundingBox assert_=== Location(-50, -30, Rectangle(100, 70))
    fixtures.simpleGroup cata boundingBox assert_=== Location(150, 70, Rectangle(350, 280))
    fixtures.complexGroup cata boundingBox assert_=== Location(30, 60, Rectangle(470, 320))
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

  test("scale works") {
    fixtures.simpleRectangle cata scale(1)  assert_=== Rectangle(80,120)
  }
}
