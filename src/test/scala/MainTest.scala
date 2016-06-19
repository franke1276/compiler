import org.scalatest._

class ExampleSpec extends FlatSpec with Matchers {

  it should "parse literal" in {
    Main.parseExtern("1").get should be(Program(ExpressionStmt(Constant(1))))
    Main.parseExtern("3 * 8").get should be(Program(ExpressionStmt(Mul(Constant(3), Constant(8)))))
    Main.parseExtern("3 * 8 * 5").get should be(Program(ExpressionStmt(Mul(Mul(Constant(3), Constant(8)), Constant(5)))))
    Main.parseExtern("3 + 8").get should be(Program(ExpressionStmt(Add(Constant(3), Constant(8)))))
    Main.parseExtern("3 + 8 + 5").get should be(Program(ExpressionStmt(Add(Add(Constant(3), Constant(8)), Constant(5)))))
    Main.parseExtern("3 + 8 * 5").get should be(Program(ExpressionStmt(Add(Constant(3), Mul(Constant(8), Constant(5))))))
    Main.parseExtern("(3 + 8) * 5").get should be(Program(ExpressionStmt(Mul(Add(Constant(3), Constant(8)), Constant(5)))))
    Main.parseExtern("2 / (3 + 8) - 5").get should be(Program(ExpressionStmt(Sub(Div(Constant(2), Add(Constant(3), Constant(8))), Constant(5)))))

    Main.parseExtern("x * 8").get should be(Program(ExpressionStmt(Mul(SymbolValue(Symbol("x")), Constant(8)))))
    Main.parseExtern("let x = 8").get should be(Program(AssignmentStmt(Symbol("x"), Constant(8))))
    Main.parseExtern("let x = 2 / (3 + 8) - 5").get should be(Program(AssignmentStmt(Symbol("x"), Sub(Div(Constant(2), Add(Constant(3), Constant(8))), Constant(5)))))
  }
}
