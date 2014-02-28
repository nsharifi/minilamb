package edu.luc.cs.cs372.team2.shapeAlgebraic

object behaviors {

  import scalamu.Algebra
  import structures._

  // TODO parsing as unfold (prefix or postfix notation)
  // TODO unparsing/printing

  // specific ShapeF-algebras: note nonrecursive nature

  val size: Algebra[ShapeF, Int] = {
    case Rectangle(_, _) => 1
    case Ellipse(_, _) => 1
    case Location(_, _, s) => 1 + s
    case Group(_, _, s @_*) => s.foldLeft(0)( (e,r) => e+r)
  }

  val depth: Algebra[ShapeF, Int] = {
    case Rectangle(_, _) => 1
    case Ellipse(_, _) => 1
    case Location(_, _, s) => 1 + s
    //case Group(_, _, s @_*) => math.max(s.)
     case Group(_, _, s @_*) => 1 + s.max
//    case Constant(_) => 1
//    case UMinus(r)   => 1 + r
//    case Plus(l, r)  => 1 + math.max(l, r)
//    case Minus(l, r) => 1 + math.max(l, r)
//    case Times(l, r) => 1 + math.max(l, r)
//    case Div(l, r)   => 1 + math.max(l, r)
//    case Mod(l, r)   => 1 + math.max(l, r)
  }
}