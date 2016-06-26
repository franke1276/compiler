import java.io.IOException

import org.apache.bcel.Constants
import org.apache.bcel.Constants.{IADD => _, IDIV => _, IMUL => _, ISUB => _, PUSH => _, _}
import org.apache.bcel.generic.{ILOAD, ISTORE, InstructionConstants, _}

import scala.collection.mutable
import scala.util.parsing.combinator._
import scalaz._
import Scalaz._
import scala.collection.generic.SeqFactory
import scala.io

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


case class FunctionCall(name: String, parameter: List[(Option[Tpe], Expression)], tpe: Option[Tpe] = None) extends Expression {
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

case class AssignmentStmt(symbol: String, tpe: Tpe, expression: Expression) extends Stmt

case class ExpressionStmt(expression: Expression) extends Stmt

case class FunctionDeclaration(name: String, params: List[(String, Tpe)], returnType: Tpe, stmts: List[Stmt]) extends Declaration

class SimpleParser extends JavaTokenParsers {
  def symbol: Parser[String] = "[a-zA-Z_]+".r

  def functionCall: Parser[FunctionCall] = functionName ~ "(" ~ repsep(expr, ",") ~ ")" ^^ {
    case fn ~ "(" ~ exp ~ ")" => FunctionCall(fn, exp.map(r => (None, r)))
  }

  def constant: Parser[Expression] = stringLiteral ^^ { s => StringConstant(s.take(s.length - 1).drop(1)) } | floatingPointNumber ^^ {
    s => Constant(s.toLong)
  } | functionCall | symbol ^^ { s => Symbol(s)}

  def factor: Parser[Expression] = constant | "(" ~> expr <~ ")"

  def expr: Parser[Expression] = (term ~ rep("+" ~ term | "-" ~ term)) ^^ {
    case t1 ~ list => list.foldLeft(t1) {
      case (x, "+" ~ y) => FunctionCall("+", List((None,x), (None,y)))
      case (x, "-" ~ y) => FunctionCall("-", List((None,x), (None,y)))
    }
  }


  def term: Parser[Expression] = (factor ~ rep("*" ~ factor | "/" ~ factor)) ^^ {
    case f1 ~ list => list.foldLeft(f1) {
      case (x, "*" ~ y) => FunctionCall("*", List((None,x), (None,y)))
      case (x, "/" ~ y) => FunctionCall("/", List((None,x), (None,y)))
    }
  }

  def tpe: Parser[Tpe] = "[A-Z][a-z0-9]*".r ^^ {
    case "Int" => IntTpe
    case "String" => StringTpe
  }

  def assignmentStmt: Parser[Stmt] = ("let" ~ symbol ~ ":" ~ tpe ~ "=" ~ expr) ^^ {
    case "let" ~ sy ~ ":" ~ t ~ "=" ~ e => AssignmentStmt(sy, t, e)
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
    parseExtern(io.Source.fromFile(args(0)).mkString)
       match {
      case Success(program, _) =>
        checkTypes(program) match {
          case \/-(p) =>
            println(p)
            gen(p)
          case -\/(errorMsg) => println(errorMsg)
        }
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

  def checkTypes(p: Program): String \/ Program = {
    var functions = mutable.Map[(String, List[Tpe]), Tpe](buildInFunctions: _*)

    def typeOf(symbols: mutable.Map[String, Tpe])(e: Expression): Tpe = {
      val tf = typeOf(symbols) _
      e match {
        case _: Constant => IntTpe
        case _: StringConstant => StringTpe
        case FunctionCall(n, args, _) if functions.get((n, args.map(a => tf(a._2)))).isDefined =>
          functions((n, args.map(a => tf(a._2))))
        case Symbol(n, _) =>
          symbols(n)
        case t => throw new IllegalStateException(s"wrong type: $t symbols: $symbols")
      }
    }
    def handleExpression(expression: Expression, getType: Expression => Tpe): String \/ Expression = {
     expression match {
       case e @ Constant(_) => \/-(e)
       case e @ StringConstant(_) => \/-(e)
       case e @ Symbol(_, _) =>
         val r  = \/-(e.copy(tpe = Some(getType(e))))
         println(s"handleExpression $r")
         r
       case e @ FunctionCall(fn, params, _) =>
         val newParams = params.map(p => handleExpression(p._2, getType).map(e => (Option(getType(p._2)), e))).sequenceU
         newParams.map(p => e.copy(tpe = Some(getType(e)), parameter = p))
     }
    }

    def handleDeclaration(e: Declaration): String \/ Declaration = {
      e match {
        case e@FunctionDeclaration(fn, params, returnType, bodyExp) =>
          functions += ((fn, params.map(_._2)) -> returnType)
          var symbols = mutable.Map[String, Tpe](params: _*)
          val tf = typeOf(symbols) _
          def handleStmt(s: Stmt): String \/ Stmt = s match {
            case a @ AssignmentStmt(symbol, t, exp) =>
              symbols += (symbol -> t)
              val expType = tf(exp)
              if (t != expType) {
                -\/(s"wrong type: $symbol has type $t but expression has type $expType")
              } else {
                val r = handleExpression(exp, tf).map( e => AssignmentStmt(symbol, t, e))
                println(r)
                r
              }
            case ExpressionStmt(exp) =>
              handleExpression(exp, tf).map( r => ExpressionStmt(r))
          }
          def checkLastExpressionAgainsReturnType(e: Stmt, rt: Tpe): Boolean = {
            e match {
              case ExpressionStmt(exp) if tf(exp) == returnType =>  true
              case ExpressionStmt(exp) => false
            }
          }

          val typedBody = bodyExp.map(handleStmt).sequenceU.flatMap(exps =>
            if (checkLastExpressionAgainsReturnType(exps.last, returnType)) {
              \/-(exps)
            } else {
              -\/(s"wrong type: expected: $returnType")
            }
          )
          println(s"symbols for $fn: $symbols")
          typedBody.map(b => e.copy(stmts = b))
      }
    }

    println(s"functions: $functions")
    p.fd.map(handleDeclaration).sequenceU.map(f => p.copy(fd = f))
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
        case Symbol(value, Some(StringTpe)) =>
          il.append(new ALOAD(vars(value)))
        case Symbol(value, Some(IntTpe)) =>
          il.append(new ILOAD(vars(value)))
        case FunctionCall("print", List((Some(t), p)), Some(rt)) =>
          val p_stream: ObjectType = new ObjectType("java.io.PrintStream")
          il.append(factory.createFieldAccess("java.lang.System", "out", p_stream, Constants.GETSTATIC))
          handleExpression(vars, mg, il)(p)
          il.append(factory.createInvoke("java.io.PrintStream", "println", javaType(rt), Array[Type](javaType(t)), Constants.INVOKEVIRTUAL))
        case FunctionCall("+", l @ List((tx,x),(ty,y)), Some(StringTpe)) =>
          il.append(factory.createNew(Type.STRINGBUFFER))
          il.append(InstructionConstants.DUP)
          handleExpression(vars, mg, il)(x)
          il.append(factory.createInvoke("java.lang.StringBuffer", "<init>", Type.VOID, Array[Type](Type.STRING), Constants.INVOKESPECIAL))
          handleExpression(vars, mg, il)(y)
          il.append(factory.createInvoke("java.lang.StringBuffer", "append", Type.STRINGBUFFER, Array[Type](Type.STRING), Constants.INVOKEVIRTUAL))
          il.append(factory.createInvoke("java.lang.StringBuffer", "toString", Type.STRING, Type.NO_ARGS, Constants.INVOKEVIRTUAL))

        case FunctionCall("+", l @ List((tx,x),(ty,y)), t) =>
          l.map(_._2).foreach(handleExpression(vars, mg, il))
          il.append(new IADD())
        case FunctionCall("-", l @ List((tx,x),(ty,y)), t) =>
          l.map(_._2).foreach(handleExpression(vars, mg, il))
          il.append(new ISUB())
        case FunctionCall("*", l @ List((tx,x),(ty,y)), t) =>
          l.map(_._2).foreach(handleExpression(vars, mg, il))
          il.append(new IMUL())
        case FunctionCall("/", l @ List((tx,x),(ty,y)), t) =>
          l.map(_._2).foreach(handleExpression(vars, mg, il))
          il.append(new IDIV())
        case a @ FunctionCall(name, params, Some(t)) =>
          params.map(_._2).foreach(handleExpression(vars, mg, il))
          il.append(factory.createInvoke("Program", name, javaType(t), params.map(t => javaType(t._1.get)).toArray, Constants.INVOKESTATIC))
      }

    }
    def handleStmt(vars: mutable.Map[String, Int], mg: MethodGen, il: InstructionList)(s: Stmt): Unit = s match {
      case AssignmentStmt(symbol, t, exp) =>
        val lg: LocalVariableGen = mg.addLocalVariable(symbol, javaType(t), null, null)
        val x: Int = lg.getIndex
        handleExpression(vars, mg, il)(exp)
        lg.setStart(il.append(new ISTORE(x)))
        vars += (symbol -> x)
      case ExpressionStmt(exp) => handleExpression(vars, mg, il)(exp)
    }

    def handleDeclaration(e: Declaration): Unit = {
      e match {
        case FunctionDeclaration("main", _, _, body) =>
          declareMethode(Type.VOID, List("argv"), List[Type](new ArrayType(Type.STRING, 1)), "main") { (il, mg, vars) =>
            body.foreach(handleStmt(vars, mg, il))
            il.append(InstructionConstants.RETURN)
          }
        case a @ FunctionDeclaration(fn, params, returnType, body) =>
          declareMethode(javaType(returnType), params.map(_._1), params.map( t => javaType(t._2)), fn) { (il, mg, vars) =>
            body.foreach(handleStmt(vars, mg, il))
            val rt = returnType match {
            case StringTpe => InstructionConstants.ARETURN
            case _ => InstructionConstants.IRETURN
          }
            il.append(rt)
          }
      }
    }
    def javaType(t: Tpe) = t match {
      case IntTpe => Type.INT
      case StringTpe => Type.STRING
      case VoidTpe => Type.VOID
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
