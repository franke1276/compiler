import org.scalatest._

import scala.util.Success

class ExampleSpec extends FlatSpec with Matchers {
  it should "parse literal" in {
    Main.parseExtern("fn main(){ 1 }").get should be(Program(List(FunctionDeclaration("main", Nil, List(ExpressionStmt(Constant(1)))))))
//    Main.parseExtern("fn main(): 3 * 8").get should be(Program(List(FunctionDeclaration("main", Nil, List(ExpressionStmt(Mul(Constant(3), Constant(8))))))))
//    Main.parseExtern("fn main(): 3 * 8 * 5").get should be(Program(List(FunctionDeclaration("main", Nil, List(ExpressionStmt(Mul(Mul(Constant(3), Constant(8)), Constant(5))))))))
//    Main.parseExtern("fn main(): 3 + 8").get should be(Program(List(FunctionDeclaration("main", Nil, List(ExpressionStmt(Add(Constant(3), Constant(8))))))))
//    Main.parseExtern("fn main(): 3 + 8 + 5").get should be(Program(List(FunctionDeclaration("main", Nil, List(ExpressionStmt(Add(Add(Constant(3), Constant(8)), Constant(5))))))))
//    Main.parseExtern("fn main(): 3 + 8 * 5").get should be(Program(List(FunctionDeclaration("main", Nil, List(ExpressionStmt(Add(Constant(3), Mul(Constant(8), Constant(5)))))))))
//    Main.parseExtern("fn main(): (3 + 8) * 5").get should be(Program(List(FunctionDeclaration("main",Nil, List(ExpressionStmt(Mul(Add(Constant(3), Constant(8)), Constant(5))))))))
//    Main.parseExtern("fn main(): 2 / (3 + 8) - 5").get should be(Program(List(FunctionDeclaration("main",Nil, List(ExpressionStmt(Sub(Div(Constant(2), Add(Constant(3), Constant(8))), Constant(5))))))))
//
//    Main.parseExtern("fn main(): x * 8").get should be(Program(List(FunctionDeclaration("main",Nil, List(ExpressionStmt(Mul(SymbolValue(Symbol("x")), Constant(8))))))))
//    Main.parseExtern("fn main(): let x = 8").get should be(Program(List(FunctionDeclaration("main",Nil, List(AssignmentStmt(Symbol("x"), Constant(8)))))))
    Main.parseExtern("fn main(){ let x = 2 / (3 + 8) - 5\n x}").get should be(Program(List(FunctionDeclaration("main",Nil, List(AssignmentStmt(Symbol("x"), Sub(Div(Constant(2), Add(Constant(3), Constant(8))), Constant(5))), ExpressionStmt(SymbolValue(Symbol("x"))))))))
//
//
//    Main.parseExtern("fn main(): 1\n2\n3").get should be(Program(List(FunctionDeclaration("main",Nil, List(ExpressionStmt(Constant(1)), ExpressionStmt(Constant(2)), ExpressionStmt(Constant(3)))))))
//    Main.parseExtern("fn main(): let x = 5 + 1\nx * 3").get should be(Program(List(FunctionDeclaration("main",Nil, List(
//      AssignmentStmt(Symbol("x"), Add(Constant(5), Constant(1))),
//      ExpressionStmt(Mul(SymbolValue(Symbol("x")), Constant(3)))
//    )))))
//
    Main.parseExtern("fn add(a,b){  a+b } fn main(){ add(5,4)}").get should be(Program(List(
      FunctionDeclaration("add", List("a", "b"), List(
        ExpressionStmt(Add(SymbolValue(Symbol("a")), SymbolValue(Symbol("b")))))),
      FunctionDeclaration("main", Nil, List(ExpressionStmt(FunctionCall("add", List(Constant(5), Constant(4))))))
    )))
//    Main.parseExtern("fn main: add(8,(4 + 7) * 2) + 1").get should be(Program(List(FunctionDeclaration("main", Nil, List(
//      ExpressionStmt(Add(FunctionCall("add", Constant(8), Mul(Add(Constant(4), Constant(7)), Constant(2)) ), Constant(1))))))))


  }
}
