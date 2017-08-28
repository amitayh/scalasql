package amitayh.scalasql

sealed trait Expression

case class BinaryOperatorApplication(operator: BinaryOperator,
                                     left: Expression,
                                     right: Expression) extends Expression

case class UnaryOperatorApplication(operator: UnaryOperator,
                                    expression: Expression) extends Expression

case class Column(name: String, table: Option[String] = None) extends Expression

case class FunctionInvocation(name: String,
                              arguments: Option[FunctionArguments] = None) extends Expression

case class ExpressionList(expressions: Expression*) extends Expression {
  assert(expressions.nonEmpty, "Expression list must contain at least one expression")
}

sealed trait Literal extends Expression

case class LiteralString(value: String) extends Literal

case class LiteralNumber[T](value: T)(implicit num: Numeric[T]) extends Literal

case object LiteralNull extends Literal

sealed trait FunctionArguments

case object Star extends FunctionArguments // TODO rename

case class ArgumentsList(distinct: Boolean, arguments: Expression*) extends FunctionArguments {
  assert(arguments.nonEmpty, "Arguments list must contain at least one expression")
}

object ArgumentsList {
  def apply(arguments: Expression*): FunctionArguments =
    ArgumentsList(distinct = false, arguments: _*)
}

case class Between(expression: Expression, lower: Expression, upper: Expression) extends Expression
