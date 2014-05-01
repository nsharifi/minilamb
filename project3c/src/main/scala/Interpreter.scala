import javax.smartcardio.TerminalFactory
import project3c.ExprFParser
import project3c.behaviors._

object Interpreter extends App {
  println("Minimal Lambda Calculus Interpreter")
  println("Type quit to exit")

  var line: String = _

  def read() = {
    print("miniLamb> ")
    line = readLine
    if (line.equals("quit")) {
      println("Goodbye!")
      TerminalFactory.getDefault()
      println("Terminal restore!")
      sys.exit() }//if
    !line.isEmpty
  }

  while (read()) {
    val result = ExprFParser.parseAll(ExprFParser.expr, line)
    if (result.isEmpty) println ("This line cannot be parsed!")
    if (result.successful) {
      println("The syntax tree is: " + result.get)
      println("It evaluates to   : " + eval(result.get))
    }
  }
}
