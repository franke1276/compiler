import scala.util.parsing.combinator._

case class Program(stmt: List[Stmt])

trait ArithmeticExpression

case class Constant(value: Long) extends ArithmeticExpression

case class Symbol(name: String)

case class SymbolValue(symbol: Symbol)  extends ArithmeticExpression

case class Mul(a: ArithmeticExpression, b: ArithmeticExpression) extends ArithmeticExpression

case class Div(a: ArithmeticExpression, b: ArithmeticExpression) extends ArithmeticExpression

case class Add(a: ArithmeticExpression, b: ArithmeticExpression) extends ArithmeticExpression

case class Sub(a: ArithmeticExpression, b: ArithmeticExpression) extends ArithmeticExpression

trait Stmt

case class AssignmentStmt(symbol: Symbol, expression: ArithmeticExpression) extends Stmt
case class ExpressionStmt(expression: ArithmeticExpression) extends Stmt


class SimpleParser extends JavaTokenParsers {
  def symbol:Parser[Symbol] = "[a-zA-Z_]+".r ^^ Symbol.apply
  def constant: Parser[ArithmeticExpression] = floatingPointNumber ^^ {
    s => Constant(s.toLong)
  } | symbol ^^ SymbolValue.apply

  def factor: Parser[ArithmeticExpression] = constant | "(" ~> expr <~ ")"

  def expr: Parser[ArithmeticExpression] = (term ~ rep("+" ~ term | "-" ~ term)) ^^ {
    case t1 ~ list => list.foldLeft(t1) {
      case (x, "+" ~ y) => Add(x, y)
      case (x, "-" ~ y) => Sub(x, y)
    }
  }


  def term: Parser[ArithmeticExpression] = (factor ~ rep("*" ~ factor | "/" ~ factor)) ^^ {
    case f1 ~ list => list.foldLeft(f1) {
      case (x, "*" ~ y) => Mul(x, y)
      case (x, "/" ~ y) => Div(x, y)
    }
  }
  def assignmentStmt: Parser[Stmt] = ("let" ~ symbol ~ "=" ~ expr) ^^ {
    case "let" ~ sy ~ "=" ~ e => AssignmentStmt(sy, e)
  }
  def exprStmt: Parser[Stmt] = expr ^^ { x => ExpressionStmt(x) }
  def stmt: Parser[Stmt] = assignmentStmt  | exprStmt
  def program: Parser[Program] = (stmt ~ rep(stmt)) ^^ {case s ~ list=> Program(s :: list)}


}


object Main extends SimpleParser {
  def parseExtern(s: String) = parse(program, s)
}
