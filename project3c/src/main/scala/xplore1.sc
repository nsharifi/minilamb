import project3c.ExprFParser

/**
 * Created by David on 4/22/14.
 */
val parser = new ExprFParser
val result = parser.parseAll(parser.exprF, "3-4*5")
if (result.successful) println(result.get)