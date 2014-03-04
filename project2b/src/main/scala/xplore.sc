import scalaz._
import Scalaz._
import scalamu._
//
type Nat = µ[Option]
def succ(n: Nat): Nat = In(Some(n))
//
val zero: Nat = In(None)
val one = succ(zero)
val two = succ(succ(zero))
val three = succ(succ(succ(zero)))

// Catamorphism
def toInt: Algebra[Option, Int] = {
  case None => 0
  case Some(n) => n + 1
}
one cata toInt assert_=== 1
two cata toInt assert_=== 2

// Anamorphism
def fromInt: Coalgebra[Option, Int] = (n: Int) => {
  if(n == 0)  None
  else        Some(n - 1)
}

µ.unfold(10)(fromInt) cata toInt assert_=== 10

def plus(m: Nat): Algebra[Option, Nat]  = {
  case None => m
  case Some(n) => succ(n)
}
two cata plus(three) cata toInt assert_=== 5
println("∆")