import java.io.IOException

import org.apache.bcel.Constants
import org.apache.bcel.Constants.{PUSH => _, _}
import org.apache.bcel.generic.{ILOAD, ISTORE, InstructionConstants, _}

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


  def main(args: Array[String]) {
    gen(Program(List(
      AssignmentStmt(
        Symbol("x"),
          Mul(
            Constant(9),
            Add(Constant(2), Constant(3))
          )),
      ExpressionStmt(
        Add(
          SymbolValue(Symbol("x")),
          Constant(9)))
    )))
    println(s"generated")
  }


  def gen(p: Program) = {
    val cg: ClassGen = new ClassGen("Program", "java.lang.Object", "<generated>", ACC_PUBLIC | ACC_SUPER, null)
    val cp: ConstantPoolGen = cg.getConstantPool
    val il: InstructionList = new InstructionList

    val mg: MethodGen = new MethodGen(ACC_STATIC | ACC_PUBLIC, Type.VOID, Array[Type](new ArrayType(Type.STRING, 1)), Array[String]("argv"), "main", "HelloWorld", il, cp)
    val factory: InstructionFactory = new InstructionFactory(cg)

    var vars = Map[String, Int]()

    def genArithmeticExpression(c: ArithmeticExpression): Unit = {
      c match {
        case Constant(value) =>
          il.append(new PUSH(cp, value.toInt))
        case SymbolValue(Symbol(value)) =>
          il.append(new ILOAD(vars(value)))
        case Add(x,y) =>
          genArithmeticExpression(x)
          genArithmeticExpression(y)
          il.append(new IADD())
        case Sub(x,y) =>
          genArithmeticExpression(x)
          genArithmeticExpression(y)
          il.append(new ISUB())
        case Mul(x,y) =>
          genArithmeticExpression(x)
          genArithmeticExpression(y)
          il.append(new IMUL())
        case Div(x,y) =>
          genArithmeticExpression(x)
          genArithmeticExpression(y)
          il.append(new IDIV())

      }
    }

    def genExpression(e: ExpressionStmt) = {
      println(s"${e.expression}")
      genArithmeticExpression(e.expression)
    }
    def genAssignment(a: AssignmentStmt) = {
      println(s"${a.symbol.name}=${a.expression}")
      val lg: LocalVariableGen = mg.addLocalVariable(a.symbol.name, Type.INT, null, null)
      val x: Int = lg.getIndex
      genArithmeticExpression(a.expression)
      lg.setStart(il.append(new ISTORE(x)))
      vars = vars + (a.symbol.name -> x)
    }



    val p_stream: ObjectType = new ObjectType("java.io.PrintStream")


    il.append(factory.createFieldAccess("java.lang.System", "out", p_stream, Constants.GETSTATIC))

    p.stmt.foreach{
      case e: ExpressionStmt => genExpression(e)
      case a: AssignmentStmt => genAssignment(a)
    }

    println(vars)

    il.append(factory.createInvoke("java.io.PrintStream", "println", Type.VOID, Array[Type](Type.INT), Constants.INVOKEVIRTUAL))

    il.append(InstructionConstants.RETURN)


    mg.setMaxStack
    cg.addMethod(mg.getMethod)
    il.dispose
    cg.addEmptyConstructor(ACC_PUBLIC)

    try {
      cg.getJavaClass.dump("Program.class")
    }
    catch {
      case e: IOException => {
        System.err.println(e)
      }
    }
  }
}
