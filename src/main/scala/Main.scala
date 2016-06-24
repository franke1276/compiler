import java.io.IOException

import org.apache.bcel.Constants
import org.apache.bcel.Constants.{IADD => _, IDIV => _, IMUL => _, ISUB => _, PUSH => _, _}
import org.apache.bcel.generic.{ILOAD, ISTORE, InstructionConstants, _}

import scala.collection.mutable
import scala.util.parsing.combinator._

case class Program(fd: List[Declaration])

trait Tpe

case object VoidTpe extends Tpe

case object IntTpe extends Tpe

case object StringTpe extends Tpe

case class CustomTpe(name: String) extends Tpe

trait Expression {
  def tpe: Option[Tpe]
}


case class Constant(value: Long) extends Expression {
  def tpe: Option[Tpe] = Some(IntTpe)

}

case class StringConstant(value: String) extends Expression {
  def tpe: Option[Tpe] = Some(StringTpe)
}


case class FunctionCall(name: String, parameter: List[Expression], tpe: Option[Tpe] = None) extends Expression {
}


case class Symbol(name: String, tpe: Option[Tpe] = None) extends Expression {
}

case class Mul(a: Expression, b: Expression, tpe: Option[Tpe] = None) extends Expression {
}

case class Div(a: Expression, b: Expression, tpe: Option[Tpe] = None) extends Expression {
}

case class Add(a: Expression, b: Expression, tpe: Option[Tpe] = None) extends Expression {
}

case class Sub(a: Expression, b: Expression, tpe: Option[Tpe] = None) extends Expression {
}

trait Stmt

trait Declaration

case class AssignmentStmt(symbol: Symbol, tpe: Option[Tpe], expression: Expression) extends Stmt

case class ExpressionStmt(expression: Expression) extends Stmt

case class FunctionDeclaration(name: String, params: List[(String, Tpe)], returnType: Tpe, stmts: List[Stmt]) extends Declaration

class SimpleParser extends JavaTokenParsers {
  def symbol: Parser[Symbol] = "[a-zA-Z_]+".r ^^ { s => Symbol(s) }

  def functionCall: Parser[FunctionCall] = functionName ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
    case fn ~ "(" ~ exp ~ ")" => FunctionCall(fn, exp)
  }

  def constant: Parser[Expression] = stringLiteral ^^ { s => StringConstant(s.take(s.length - 1).drop(1)) } | floatingPointNumber ^^ {
    s => Constant(s.toLong)
  } | functionCall | symbol

  def factor: Parser[Expression] = constant | "(" ~> expr <~ ")"

  def expr: Parser[Expression] = (term ~ rep("+" ~ term | "-" ~ term)) ^^ {
    case t1 ~ list => list.foldLeft(t1) {
      case (x, "+" ~ y) => FunctionCall("+", List(x, y))
      case (x, "-" ~ y) => FunctionCall("-", List(x, y))
    }
  }


  def term: Parser[Expression] = (factor ~ rep("*" ~ factor | "/" ~ factor)) ^^ {
    case f1 ~ list => list.foldLeft(f1) {
      case (x, "*" ~ y) => FunctionCall("*", List(x, y))
      case (x, "/" ~ y) => FunctionCall("/", List(x, y))
    }
  }

  def tpe: Parser[Tpe] = "[A-Z][a-z0-9]*".r ^^ {
    case "Int" => IntTpe
    case "String" => StringTpe
  }

  def assignmentStmt: Parser[Stmt] = ("let" ~ symbol ~ opt(":" ~ tpe) ~ "=" ~ expr) ^^ {
    case "let" ~ sy ~ None ~ "=" ~ e => AssignmentStmt(sy, None, e)
    case "let" ~ sy ~ Some(":" ~ t) ~ "=" ~ e => AssignmentStmt(sy, Some(t), e)
  }

  def exprStmt: Parser[Stmt] = expr ^^ { x => ExpressionStmt(x) }

  def functionName: Parser[String] = "[a-zA-Y_]+".r

  def functionParam: Parser[(String, Tpe)] = ("[a-zA-Y_]+".r ~ ":" ~ typeName) ^^ {
    case pn ~ ":" ~ ptn => (pn, ptn)
  }

  def functionBody: Parser[List[Stmt]] = stmts

  def typeName: Parser[Tpe] = "[a-zA-Y_]+".r ^^ {
    case "Int" => IntTpe
    case "String" => StringTpe
    case "Void" => VoidTpe
  }

  def functionDeclarationStmt: Parser[Declaration] = ("fn" ~ functionName ~
    "(" ~ repsep(functionParam, ",") ~ ")" ~ ":" ~ typeName ~ "{" ~ functionBody ~ "}") ^^ {
    case "fn" ~ fn ~ "(" ~ fps ~ ")" ~ ":" ~ tn ~ "{" ~ fb ~ "}" => FunctionDeclaration(fn, fps, tn, fb)
  }


  def stmt: Parser[Stmt] = assignmentStmt | exprStmt

  def stmts: Parser[List[Stmt]] = (stmt ~ rep(stmt)) ^^ { case s ~ list => s :: list }

  def program: Parser[Program] = (functionDeclarationStmt ~ rep(functionDeclarationStmt)) ^^ { case fd ~ fds => Program(fd :: fds) }


}


object Main extends SimpleParser {
  def parseExtern(s: String) = parse(program, s)


  def main(args: Array[String]) {
    parseExtern(
      """fn hello(s: String): String {
        |s + "halle"
        |}
        |fn add(a: Int,b: Int): Int{
        | let x: Int = a + 7
        | x + b / 3
        | }
        |fn main(): Void{
        | print(add(4 * (5 + 7),7))
        |}
      """.stripMargin) match {
      case Success(program, _) =>
        println(program)

        checkTypes(program)

        gen(program)
      case e => println(e)
    }
    println(s"generated")
  }

  val buildInFunctions: List[((String, List[Tpe]), Tpe)] = List(
    (("+", List[Tpe](IntTpe, IntTpe)), IntTpe),
    (("-", List[Tpe](IntTpe, IntTpe)), IntTpe),
    (("*", List[Tpe](IntTpe, IntTpe)), IntTpe),
    (("/", List[Tpe](IntTpe, IntTpe)), IntTpe),
    (("+", List[Tpe](StringTpe, StringTpe)), StringTpe),
    (("print", List[Tpe](StringTpe)), VoidTpe),
    (("print", List[Tpe](IntTpe)), VoidTpe)
  )

  def checkTypes(p: Program) = {
    var functions = mutable.Map[(String, List[Tpe]), Tpe](buildInFunctions: _*)

    def typeOf(symbols: mutable.Map[String, Tpe])(e: Expression): Tpe = {
      val tf = typeOf(symbols) _
      e match {
        case _: Constant => IntTpe
        case _: StringConstant => StringTpe
        case FunctionCall(n, args, _) if functions.get((n, args.map(tf))).isDefined =>
          functions((n, args.map(tf)))
        case Symbol(n, _) =>
          symbols(n)
        case t => throw new IllegalStateException(s"wrong type: $t symbols: $symbols")
      }
    }

    def handleDeclaration(e: Declaration): Unit = {
      e match {
        case e@FunctionDeclaration(fn, params, returnType, body) =>
          functions += ((fn, params.map(_._2)) -> returnType)
          var symbols = mutable.Map[String, Tpe](params: _*)
          val tf = typeOf(symbols) _
          def handleStmt(s: Stmt): Unit = s match {
            case AssignmentStmt(symbol, Some(t), exp) =>
              symbols += (symbol.name -> t)
              val expType = tf(exp)
              if (t != expType) {
                throw new IllegalStateException(s"wrong type: $symbol has type $t but expression has type $expType")
              }
            case AssignmentStmt(symbol, None, exp) =>
              symbols += (symbol.name -> tf(exp))
            case ExpressionStmt(exp) =>
          }
          body.foreach(handleStmt)
          body.last match {
            case ExpressionStmt(exp) if tf(exp) == returnType =>
            case ExpressionStmt(exp) => throw new IllegalStateException(s"type of last expression (${tf(exp)} is incompatible with return type of the function : $returnType)")
          }
          println(s"symbols for $fn: $symbols")
      }
    }

    p.fd.foreach(handleDeclaration)
    println(s"functions: $functions")
  }

  def gen(p: Program) = {
    val cg: ClassGen = new ClassGen("Program", "java.lang.Object", "<generated>", ACC_PUBLIC | ACC_SUPER, null)
    val cp: ConstantPoolGen = cg.getConstantPool
    val factory: InstructionFactory = new InstructionFactory(cg)

    def declareMethode(returnType: Type, argNames: List[String], argType: List[Type], methodName: String)(f: (InstructionList, MethodGen, mutable.Map[String, Int]) => Unit): Unit = {
      val il: InstructionList = new InstructionList
      val mg = new MethodGen(ACC_STATIC | ACC_PUBLIC, returnType, argType.toArray, argNames.toArray, methodName, "Program", il, cp)
      val vars = mutable.Map[String, Int](argNames.zipWithIndex: _*)
      f(il, mg, vars)
      mg.setMaxStack
      cg.addMethod(mg.getMethod)
      il.dispose
    }

    def handleExpression(vars: mutable.Map[String, Int], mg: MethodGen, il: InstructionList)(e: Expression): Unit = {
      e match {
        case Constant(value) =>
          il.append(new PUSH(cp, value.toInt))
        case StringConstant(value) =>
          il.append(new PUSH(cp, value))
        case FunctionCall("print", List(p), t) =>
          val p_stream: ObjectType = new ObjectType("java.io.PrintStream")
          il.append(factory.createFieldAccess("java.lang.System", "out", p_stream, Constants.GETSTATIC))
          handleExpression(vars, mg, il)(p)
          il.append(factory.createInvoke("java.io.PrintStream", "println", Type.VOID, Array[Type](Type.INT), Constants.INVOKEVIRTUAL))
        case Symbol(value, _) =>
          il.append(new ILOAD(vars(value)))
        case FunctionCall("+", l @ List(x,y), t) =>
          l.foreach(handleExpression(vars, mg, il))
          il.append(new IADD())
        case FunctionCall("-", l @ List(x,y), t) =>
          l.foreach(handleExpression(vars, mg, il))
          il.append(new ISUB())
        case FunctionCall("*", l @ List(x,y), t) =>
          l.foreach(handleExpression(vars, mg, il))
          il.append(new IMUL())
        case FunctionCall("/", l @ List(x,y), t) =>
          l.foreach(handleExpression(vars, mg, il))
          il.append(new IDIV())
        case FunctionCall(name, params, t) =>
          params.foreach(handleExpression(vars, mg, il))
          il.append(factory.createInvoke("Program", name, Type.INT, params.map(_ => Type.INT).toArray, Constants.INVOKESTATIC))

      }

    }
    def handleStmt(vars: mutable.Map[String, Int], mg: MethodGen, il: InstructionList)(s: Stmt): Unit = s match {
      case AssignmentStmt(symbol, t, exp) =>
        val lg: LocalVariableGen = mg.addLocalVariable(symbol.name, Type.INT, null, null)
        val x: Int = lg.getIndex
        handleExpression(vars, mg, il)(exp)
        lg.setStart(il.append(new ISTORE(x)))
        vars += (symbol.name -> x)
      case ExpressionStmt(exp) => handleExpression(vars, mg, il)(exp)
    }

    def handleDeclaration(e: Declaration): Unit = {
      e match {
        case FunctionDeclaration("main", _, _, body) =>
          declareMethode(Type.VOID, List("argv"), List[Type](new ArrayType(Type.STRING, 1)), "main") { (il, mg, vars) =>
            body.foreach(handleStmt(vars, mg, il))
            il.append(InstructionConstants.RETURN)
          }
        case FunctionDeclaration(fn, params, returnType, body) =>
          declareMethode(Type.INT, params.map(_._1), params.map(_ => Type.INT), fn) { (il, mg, vars) =>
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
