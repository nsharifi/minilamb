package project3c
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import project3c.structures._
import scalaz.syntax.show._
import scalamu.In

import structures.ExprFactory._
import scala.util.matching.Regex

object ExprFParser extends StandardTokenParsers {

  def regex(r: Regex): Parser[String] = new Parser[String] {
    def apply(in: Input) = r.findPrefixMatchOf(
      in.source.subSequence(in.offset, in.source.length)) match {
      case Some(matched) =>
        Success(in.source.subSequence(in.offset,
          in.offset + matched.end).toString, in.drop(matched.end))
      case None => Failure("string matching regex `" + r +
        "' expected but " + in.first + " found", in)
    }
  }

  lexical.delimiters += ("(", ")", "+", "-", "*", "/", "%", ".", "::")
  lexical.reserved += ("if", "then", "else", "λ", "lambda", "nil")

  def expr: Parser[Expr] =
      term ~! opt(("+" | "-") ~ term) ^^ {

      case l ~ None => l
      case l ~ Some("+" ~ r) => plus(l, r)
      case l ~ Some("-" ~ r) => minus(l, r)
    }

  /** term ::= factor { { "*" | "/" } factor }%* */
  def term: Parser[Expr] =
    factor ~! opt(("*" | "/" | "%") ~ factor) ^^ {
      case l ~ None => l
      case l ~ Some("*" ~ r) => times(l, r)
      case l ~ Some("/" ~ r) => div(l, r)
      case l ~ Some("%" ~ r) => mod(l, r)
    }

  val vari = "[a-zA-Z_]+[a-zA-Z_0-9]*".r
  /** factor ::= numericLit | "+" factor | "-" factor | "(" expr ")" */
  def factor: Parser[Expr] = (
    numericLit ^^ { case s => constant(s.toInt) }
    | "(" ~> exprs <~ ")" ^^ { case e => e }
    | "+" ~> factor ^^ { case e => e }
    | "-" ~> factor ^^ { case e => uminus(e) }
    | ident ^^ { case v => variable(v)}
    | ("lambda" | "λ") ~> ident ~ "." ~ exprs ^^ { case v~_~e => fun(v, e) }
    | ("lambda" | "λ") ~> ident ~ "." ~ ident ^^ { case v1~_~v2 => fun(v1, variable(v2)) }
    |  "if" ~ exprs ~ "then" ~ exprs ~ "else" ~ exprs ^^ { case _~c~_~t~_~e => iff(c, t, e) }
    | "(" ~> numericLit ~ rep1("::" ~> (numericLit|"nil")) <~ ")" ^^ {
      case h~t => t.foldLeft (cell(constant(h.toInt), constant(0))) ( (v, l) => {
        (v, l) match {
          case (In(Cell(head, tail)), "nil") => cell(head, constant(0))
          case (In(Cell(head, In(Constant(0)))), _) => cell(head, constant(l.toInt))
          case (In(Cell(head, tail)), _)     => cell(cell(head, tail), constant(l.toInt))
        }

      })
    }

    )

  def exprs: Parser[Expr] =
    expr ~ rep(expr) ^^ {
      case e1 ~ List() => e1
      case e1 ~ e2 => e2.foldLeft (app(e1, e1)) ( (res, nxt) => {
        (res, nxt) match {
          case (In(App(`e1`, `e1`)), _) => app(e1, nxt)
          case (In(App(elem1, elem2)), _) => app(app(elem1, elem2), nxt)
        }
      })
    }

  def parseAll[T](p: Parser[T], in: String): ParseResult[T] =
    phrase(p)(new lexical.Scanner(in))

}

