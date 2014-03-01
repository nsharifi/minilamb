package edu.luc.cs.cs372.team2.shapeAlgebraic

object behaviors {

  import scalamu._
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

  // TODO test this
  // FIXME take a close look at Group case
  def scale(fctr: Int): Algebra[ShapeF, Shape] = {
    case Rectangle(w, h)   => In(Rectangle(w*fctr, h*fctr))
    case Ellipse(a, b)     => In(Ellipse(a*fctr, b*fctr))
    case Location(x, y, s) => In(Location(x*fctr, y*fctr,In(
                                 Rectangle(s.asInstanceOf[Rectangle].width*fctr,
                                    s.asInstanceOf[Rectangle].height*fctr))))
    case Group(s @_*)      => In(Group(s: _*))
  }

//  val boundingBox: Algebra[ShapeF, Rectangle]  = {
//    case Rectangle(_, _) => Location(0, 0, )
//
//  }
}