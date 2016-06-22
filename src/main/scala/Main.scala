import java.io.IOException

import org.apache.bcel.Constants
import org.apache.bcel.Constants.{IADD => _, IDIV => _, IMUL => _, ISUB => _, PUSH => _, _}
import org.apache.bcel.generic.{ILOAD, ISTORE, InstructionConstants, _}

import scala.collection.mutable
import scala.util.parsing.combinator._

case class Program(fd: List[Declaration])

trait Expression

case class Constant(value: Long) extends Expression

case class FunctionCall(name: String, parameter: List[Expression]) extends Expression

case class Symbol(name: String)

case class SymbolValue(symbol: Symbol)  extends Expression

case class Mul(a: Expression, b: Expression) extends Expression

case class Div(a: Expression, b: Expression) extends Expression

case class Add(a: Expression, b: Expression) extends Expression

case class Sub(a: Expression, b: Expression) extends Expression

trait Stmt
trait Declaration

case class AssignmentStmt(symbol: Symbol, expression: Expression) extends Stmt
case class ExpressionStmt(expression: Expression) extends Stmt
case class FunctionDeclaration(name: String, params: List[String], stmts: List[Stmt]) extends Declaration

class SimpleParser extends JavaTokenParsers {
  def symbol:Parser[Symbol] = "[a-zA-Z_]+".r ^^ Symbol.apply
  def functionCall: Parser[FunctionCall]=  functionName ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
    case fn ~ "(" ~ exp ~ ")" => FunctionCall(fn, exp)
  }
  def constant: Parser[Expression] = floatingPointNumber ^^ {
    s => Constant(s.toLong)
  } | functionCall | symbol ^^ SymbolValue.apply

  def factor: Parser[Expression] = constant | "(" ~> expr <~ ")"

  def expr: Parser[Expression] = (term ~ rep("+" ~ term | "-" ~ term)) ^^ {
    case t1 ~ list => list.foldLeft(t1) {
      case (x, "+" ~ y) => Add(x, y)
      case (x, "-" ~ y) => Sub(x, y)
    }
  }


  def term: Parser[Expression] = (factor ~ rep("*" ~ factor | "/" ~ factor)) ^^ {
    case f1 ~ list => list.foldLeft(f1) {
      case (x, "*" ~ y) => Mul(x, y)
      case (x, "/" ~ y) => Div(x, y)
    }
  }
  def assignmentStmt: Parser[Stmt] = ("let" ~ symbol ~ "=" ~ expr) ^^ {
    case "let" ~ sy ~ "=" ~ e => AssignmentStmt(sy, e)
  }
  def exprStmt: Parser[Stmt] = expr ^^ { x => ExpressionStmt(x) }
  def functionName: Parser[String] = "[a-zA-Y_]+".r
  def functionParam: Parser[String] = "[a-zA-Y_]+".r
  def functionBody: Parser[List[Stmt]] = stmts
  def functionDeclarationStmt: Parser[Declaration] = ("fn" ~ functionName ~
    "(" ~ repsep(functionParam, ",") ~ ")" ~ "{" ~ functionBody ~ "}") ^^ {
    case "fn" ~ fn ~ "(" ~ fps ~ ")" ~ "{" ~ fb ~ "}" => FunctionDeclaration(fn, fps, fb)
  }


  def stmt: Parser[Stmt] = assignmentStmt | exprStmt
  def stmts: Parser[List[Stmt]] = (stmt ~ rep(stmt)) ^^ {case s ~ list=> s :: list}
  def program: Parser[Program] = (functionDeclarationStmt ~ rep(functionDeclarationStmt)) ^^ {case fd ~ fds => Program(fd :: fds)}



}


object Main extends SimpleParser {
  def parseExtern(s: String) = parse(program, s)


  def main(args: Array[String]) {
    parseExtern(
      """fn add(a,b){
        | a + b
        | }
        |fn main(){
        | print(add(4,7))
        |}
      """.stripMargin) match {
      case Success(program, _) =>
        println(program)
        gen(program)
      case e =>  println(e)
    }
    println(s"generated")
  }


  def gen(p: Program) = {
    val cg: ClassGen = new ClassGen("Program", "java.lang.Object", "<generated>", ACC_PUBLIC | ACC_SUPER, null)
    val cp: ConstantPoolGen = cg.getConstantPool
    val factory: InstructionFactory = new InstructionFactory(cg)

    def declareMethode(returnType: Type, argNames: List[String], argType: List[Type], methodName: String)(f: (InstructionList, MethodGen, mutable.Map[String, Int]) => Unit): Unit={
      val il: InstructionList = new InstructionList
      val mg = new MethodGen(ACC_STATIC | ACC_PUBLIC, returnType, argType.toArray, argNames.toArray, methodName, "Program", il, cp)
      val vars = mutable.Map[String, Int](argNames.zipWithIndex:_*)
     f(il, mg, vars)
      mg.setMaxStack
      cg.addMethod(mg.getMethod)
      il.dispose
    }

    def handleExpression(vars: mutable.Map[String, Int], mg: MethodGen, il: InstructionList)(e: Expression): Unit = {
      e match {
        case Constant(value) =>
          il.append(new PUSH(cp, value.toInt))
        case FunctionCall("print", params) =>
          val p_stream: ObjectType = new ObjectType("java.io.PrintStream")
          il.append(factory.createFieldAccess("java.lang.System", "out", p_stream, Constants.GETSTATIC))
          params.foreach(handleExpression(vars, mg, il))
          il.append(factory.createInvoke("java.io.PrintStream", "println", Type.VOID, Array[Type](Type.INT), Constants.INVOKEVIRTUAL))
        case FunctionCall(name, params) =>
          params.foreach(handleExpression(vars, mg, il))
          il.append(factory.createInvoke("Program", name, Type.INT, params.map(_ => Type.INT).toArray , Constants.INVOKESTATIC))
        case SymbolValue(Symbol(value)) =>
          il.append(new ILOAD(vars(value)))
        case Add(x, y) =>
          Seq(x,y).foreach(handleExpression(vars, mg, il))
          il.append(new IADD())
        case Sub(x, y) =>
          Seq(x,y).foreach(handleExpression(vars, mg, il))
          il.append(new ISUB())
        case Mul(x, y) =>
          Seq(x,y).foreach(handleExpression(vars, mg, il))
          il.append(new IMUL())
        case Div(x, y) =>
          Seq(x,y).foreach(handleExpression(vars, mg, il))
          il.append(new IDIV())

      }

    }
   def handleStmt(vars: mutable.Map[String, Int], mg: MethodGen, il: InstructionList)(s: Stmt): Unit = s match {
     case AssignmentStmt(symbol, exp) =>
       val lg: LocalVariableGen = mg.addLocalVariable(symbol.name, Type.INT, null, null)
       val x: Int = lg.getIndex
       handleExpression(vars, mg, il)(exp)
       lg.setStart(il.append(new ISTORE(x)))
       vars += (symbol.name -> x)
     case ExpressionStmt(exp) => handleExpression(vars, mg, il)(exp)
   }

    def handleDeclaration(e: Declaration): Unit = {
      e match {
        case FunctionDeclaration("main" ,_, body) =>
          declareMethode(Type.VOID, List("argv"), List[Type](new ArrayType(Type.STRING, 1)),"main"){ (il, mg, vars) =>
            body.foreach(handleStmt(vars, mg, il))
            il.append(InstructionConstants.RETURN)
          }
        case FunctionDeclaration(fn, params, body) =>
          declareMethode(Type.INT, params,params.map(_ => Type.INT) ,fn){ (il, mg, vars) =>
           body.foreach(handleStmt(vars, mg, il))
            il.append(InstructionConstants.IRETURN)
          }
      }
    }
    p.fd.foreach(handleDeclaration)
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
