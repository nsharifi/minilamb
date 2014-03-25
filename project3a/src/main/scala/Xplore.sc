println("@")

import scalamu.ToMuOps
import project3a.behaviors._

eval(Var("x")) -> error


eval(Fun("x", Plus(Constant(7), Var("x"))))


-> Fun("x", Plus(Const(7), Var("x")))


eval(App(Fun("x", Plus(Const(7), Var("x"))), Const(3)))


-> Const(10)


eval(App(Var("x"), Const(3)) -> error
  eval(If(Const(7), Const(3), Const(4))) -> Const(3)
  eval(If(Const(0), Const(3), Const(4))) -> Const(4)
  eval(If(Fun("x", Var("x")), Const(3), Const(4))) -> Const(3)
  eval(If(Fun("x", Var("y")), Const(3), Const(4))) -> Const(3)


val Y = // you'll define it yourself as in the handout!
  eval(App(App(Y, Fun("f", Fun("n", If(Var("n"),
    Times(Var("n"), App(Var("f"), Minus(Var("n"), Const(1)))), Const(1))))),
    Const(5)))


-> Const(120)

