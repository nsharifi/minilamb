package edu.luc.cs.cs372.team2.shapeAlgebraic

object behaviors {

  import scalamu.Algebra
  import structures._

  // specific ShapeF-algebras: note nonrecursive nature

  def size: Algebra[ShapeF, Int] = {
    case Rectangle(_, _) => 1
    case Ellipse(_, _) => 1
    case Location(_, _, s) => s
    case Group(s @_*) => s.sum
  }

  def depth: Algebra[ShapeF, Int] = {
    case Rectangle(_, _) => 1
    case Ellipse(_, _) => 1
    case Location(_, _, s) => s + 1
    case Group(s @_*) => s.max + 1
  }

//  def scale: Algebra[ShapeF, Int] = {
//    case Rectangle(_, _) => 1
//    case Ellipse(_, _) => 1
//    case Location(_, _, s) => s + 1
//    case Group(s @_*) => s.max + 1
//  }

//  val boundingBox: Algebra[ShapeF, Rectangle]  = {
//    case Rectangle(_, _) => Location(0, 0, )
//
//  }
}