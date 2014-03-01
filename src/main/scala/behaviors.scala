package edu.luc.cs.cs372.team2.shapeAlgebraic

object behaviors {

  import scalamu.Algebra
  import structures._

  // specific ShapeF-algebras: note nonrecursive nature

  def size: Algebra[ShapeF, Int] = {
    case Rectangle(_, _) => 1
    case Ellipse(_, _) => 1
    case Location(_, _, s) => s
    //case Group(s @_*) => s.foldLeft(0)( (e,r) => e+r)
    case Group(s @_*) => s.sum
  }

  def depth: Algebra[ShapeF, Int] = {
    case Rectangle(_, _) => 1
    case Ellipse(_, _) => 1
<<<<<<< local
    case Location(_, _, s) => s
    case Group(s @_*) => s.sum
=======
    case Location(_, _, s) => 1 + s
    //case Group(_, _, s @_*) => math.max(s.)
     case Group(s @_*) => 1 + s.max
>>>>>>> other
  }

  def scale: Algebra[ShapeF, Int] = {
    case Rectangle(_, _) => Rectangle(_,_)
    case Ellipse(_, _) => 1
<<<<<<< local
    case Location(_, _, s) => s + 1
    case Group(s @_*) => s.max + 1
=======
    case Location(_,_,s) => Location(factor*x, factor*y, scale(s, f))
    case Group(s @ _*) =>  Group(s.map(scale(_, f)):_*)
>>>>>>> other
  }

//  val boundingBox: Algebra[ShapeF, Rectangle]  = {
//    case Rectangle(_, _) => Location(0, 0, )
//
//  }
}