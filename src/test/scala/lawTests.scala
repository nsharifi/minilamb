package edu.luc.cs.cs372.team2.shapeAlgebraic

import org.scalatest.FunSuite
import scalaz.syntax.equal._
import scalaz.std.anyVal._
import scalamu._

class lawTests extends FunSuite {

  import structures._
  import structures.ShapeFactory._

  test("equality works") {
    (Ellipse(3, 2): ShapeF[Int]) assert_=== (Ellipse(3, 2): ShapeF[Int])
    ellipse(3, 2) assert_=== ellipse(3, 2)
//    (Location(10, 20, Ellipse(3, 2))) assert_=== (Location(10, 20, Ellipse(3, 2)))
  }

//  test("equality and functor laws hold for ExprF") {
    import scalaz.syntax.functor._
    import scalaz.scalacheck.ScalazArbitrary._
    import scalaz.scalacheck.ScalaCheckBinding._
    import scalaz.scalacheck.ScalazProperties._
    import org.scalacheck.Arbitrary

//    implicit def ShapeFArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[ShapeF[A]] =
//      a map { a => (Plus(a, a): ExprF[A]) }
//
//    // TODO figure out how to integrate these checks better with ScalaTest
//    //      so that the test fails if one of the checks does
//    equal.laws[ExprF[Int]].check
//    functor.laws[ExprF].check
//  }
}