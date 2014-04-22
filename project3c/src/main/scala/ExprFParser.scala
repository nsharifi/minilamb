package project3c
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import project3c.structures._
//import scalaz.syntax.show._


class ExprFParser extends StandardTokenParsers {

  lexical.delimiters += ("(", ")", "+", "-", "*", "/")

  def exprF: Parser[ExprF] =
      term ~! opt(("+" | "-") ~ exprF) ^^ {
      case l ~ None => l
      case l ~ Some("+" ~ r) => Plus(l, r)
      case l ~ Some("-" ~ r) => Minus(l, r)
    }

  /** term ::= factor { { "*" | "/" } factor }%* */
  def term: Parser[ExprF] =
    factor ~! opt(("*" | "/") ~ term) ^^ {
      case l ~ None => l
      case l ~ Some("*" ~ r) => Times(l, r)
      case l ~ Some("/" ~ r) => Div(l, r)
      case l ~ Some("%" ~ r) => Mod(l, r)
    }

  /** factor ::= numericLit | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[ExprF] = (
    numericLit ^^ { case s => Constant(s.toInt) }
      | "+" ~> factor ^^ { case e => e }
      | "-" ~> factor ^^ { case e => UMinus(e) }
      | "(" ~ exprF ~ ")" ^^ { case _ ~ e ~ _ => e }
    // | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    )



  def parseAll[T](p: Parser[T], in: String): ParseResult[T] =
    phrase(p)(new lexical.Scanner(in))

}
