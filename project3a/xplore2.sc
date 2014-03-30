//http://deptinfo.unice.fr/~roy/sicp.pdf
//http://wqzhang.wordpress.com/sicp-solutions/
/*Exercise 1.41. Define a procedure double that takes a procedure of one argument as argument and
  returns a procedure that applies the original procedure twice. For example, if inc is a procedure that adds
  1 to its argument, then (double inc) should be a procedure that adds 2. What value is returned by
  (define (double f)
  (lambda (x) (f (f x))))

(define (inc x)
  (+ x 1))

(((double (double double)) inc) 5)
;Value 21 (5+1)+(6+1)+(7+1)

  */
def sqr(x: Int) = x*x
def cube(x: Int) = x*x*x
def double(f: Int=>Int): Int=>Int = x => f(f(x))
def inc(x:Int)= x+1
//val f1 = inc(5)
val f3 = double(inc)
f3(5)
val f2 = double(double(double(inc)))
println(f2(5))

val f4 = double(double(double(double(inc))))
f4(5)
/*Exercise 1.42. Let f and g be two one-argument functions. The composition f after g is defined to be the
  function x f(g(x)). Define a procedure compose that implements composition. For example, if inc is a
procedure that adds 1 to its argument,
(define (compose f g)
  (lambda (x) (f (g x))))
*/
def compose(f: Int=>Int, g: Int=>Int): Int=>Int = x => f(g(x))
val fA = compose(sqr, inc)
fA(6)

def Indentity(x:Int)=x
Indentity(10)

def Var_X(x:_):_={
  if(x=='x') "error"
  else -1
}
//if lhs = var x--->error
