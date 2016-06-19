import org.scalatest._

class ExampleSpec extends FlatSpec with Matchers {

  it should "parse literal" in {
    Main.parseExtern("1").get should be(Program(List(ExpressionStmt(Constant(1)))))
    Main.parseExtern("3 * 8").get should be(Program(List(ExpressionStmt(Mul(Constant(3), Constant(8))))))
    Main.parseExtern("3 * 8 * 5").get should be(Program(List(ExpressionStmt(Mul(Mul(Constant(3), Constant(8)), Constant(5))))))
    Main.parseExtern("3 + 8").get should be(Program(List(ExpressionStmt(Add(Constant(3), Constant(8))))))
    Main.parseExtern("3 + 8 + 5").get should be(Program(List(ExpressionStmt(Add(Add(Constant(3), Constant(8)), Constant(5))))))
    Main.parseExtern("3 + 8 * 5").get should be(Program(List(ExpressionStmt(Add(Constant(3), Mul(Constant(8), Constant(5)))))))
    Main.parseExtern("(3 + 8) * 5").get should be(Program(List(ExpressionStmt(Mul(Add(Constant(3), Constant(8)), Constant(5))))))
    Main.parseExtern("2 / (3 + 8) - 5").get should be(Program(List(ExpressionStmt(Sub(Div(Constant(2), Add(Constant(3), Constant(8))), Constant(5))))))

    Main.parseExtern("x * 8").get should be(Program(List(ExpressionStmt(Mul(SymbolValue(Symbol("x")), Constant(8))))))
    Main.parseExtern("let x = 8").get should be(Program(List(AssignmentStmt(Symbol("x"), Constant(8)))))
    Main.parseExtern("let x = 2 / (3 + 8) - 5").get should be(Program(List(AssignmentStmt(Symbol("x"), Sub(Div(Constant(2), Add(Constant(3), Constant(8))), Constant(5))))))


    Main.parseExtern("1\n2\n3").get should be(Program(List(ExpressionStmt(Constant(1)), ExpressionStmt(Constant(2)), ExpressionStmt(Constant(3)))))
    Main.parseExtern("let x = 5 + 1\nx * 3").get should be(Program(List(
      AssignmentStmt(Symbol("x"), Add(Constant(5), Constant(1))),
      ExpressionStmt(Mul(SymbolValue(Symbol("x")), Constant(3)))
    )))

  }
}
