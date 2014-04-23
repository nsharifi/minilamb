package project3c
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import project3c.structures._
import scalaz.syntax.show._
import scalamu.In


class ExprFParser extends StandardTokenParsers {

  lexical.delimiters += ("(", ")", "+", "-", "*", "/")

  def expr: Parser[Expr] =
      term ~! opt(("+" | "-") ~ expr) ^^ {
      case l ~ None => l
      case l ~ Some("+" ~ r) => In(Plus(l, r))
      case l ~ Some("-" ~ r) => In(Minus(l, r))
    }

  /** term ::= factor { { "*" | "/" } factor }%* */
  def term: Parser[Expr] =
    factor ~! opt(("*" | "/") ~ term) ^^ {
      case l ~ None => l
      case l ~ Some("*" ~ r) => In(Times(l, r))
      case l ~ Some("/" ~ r) => In(Div(l, r))
      case l ~ Some("%" ~ r) => In(Mod(l, r))
    }

  /** factor ::= numericLit | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    numericLit ^^ { case s => In(Constant(s.toInt)) }
      | "+" ~> factor ^^ { case e => e }
      | "-" ~> factor ^^ { case e => In(UMinus(e)) }
      | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    // | "(" ~ expr ~ ")" ^^ { case _ ~ e ~ _ => e }
    )

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] =
    phrase(p)(new lexical.Scanner(in))

}
