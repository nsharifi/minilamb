import scala.util.parsing.combinator.RegexParsers
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import scalaz.syntax.show._

class Expr
case class Number(value: String) extends Expr
case class Operator(op: String, left: Expr, right: Expr) extends Expr

class ExprParser extends RegexParsers {
  val number = "[0-9]".r

  def expr: Parser[Expr]   = term ~ opt(("+" | "-") ~ expr) ^^ {
    case t ~ None => t
    case t1 ~ Some("+" ~ t2) => Operator("+", t1, t2)
    case t1 ~ Some("-" ~ t2) => Operator("-", t1, t2)
  }

  def term: Parser[Expr]   = (factor ~ opt("*" ~> term)) ^^ {
    case f1 ~ None => f1
    case f1 ~ Some(f2) => Operator("*", f1, f2)
  }

  def factor: Parser[Expr] = number ^^
    (n => Number(n.toString)) |
    "(" ~> expr <~ ")"

}

val parser = new ExprParser
val res = parser.parseAll(parser.expr, "(2+4)*3")/*[1.8] parsed: Operator(*,Operator(+,Number(2),Nu

  mber(4)),Number(3))*/


val res2 = parser.parseAll(parser.expr, "(3+4*5)")

println(res2)/*> [1.8] parsed: Operator(+,Number(3),Operator(*,Number(4),Number(5)))*/





println("•")
println(res.get)
println("•")


//    case t ~ None => t
//    case t1 ~ Some("+" ~ t2) => t1 + t2
//    case t1 ~ Some("-" ~ t2) => t1 - t2
//    case f1 ~ f2 => f1 * f2.product
//  def term: Parser[Int]   = factor ~ rep(("*") ~ factor) ^^ {
//    case f1 ~ f2 => f1 * f2.map(_._2).product
//  }
//  number ^^ {_.toInt} |
//  "(" ~> expr <~ ")" ^^  { case e => e}
//  def factor: Parser[Int] = number ^^ {_.toInt} |
//    "(" ~ expr ~ ")" ^^  { case _ ~ e ~ _ => e}
