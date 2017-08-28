package amitayh.scalasql

trait Printer[A] {
  def print(a: A): String
}

object Printer {

  def qualifiedName(name: String): String = '"' + name + '"'

  def printSelect(select: Select): String = {
    val resultColumns = select.columns.map(printResultColumn)
    val base =
      s"""
         |SELECT
         |${resultColumns.mkString(",\n").indent(2)}
         |FROM
         |${printTableOrSubquery(select.from).indent(2)}""".stripMargin.trim

    base +
      printConditionClause("WHERE", select.where) +
      printGroupByClause(select.groupBy) +
      printConditionClause("HAVING", select.having)
  }

  def printResultColumn(column: ResultColumn): String = column match {
    case All => "*"
    case ExpressionColumn(expression, alias) =>
      s"${printExpression(expression)}${printAlias(alias)}"
  }

  def printTableOrSubquery(tableOrSubquery: TableOrSubquery): String = tableOrSubquery match {
    case Table(name, schema, alias) =>
      val schemaString = schema.map(schema => s"${qualifiedName(schema)}.").getOrElse("")
      s"$schemaString${qualifiedName(name)}${printAlias(alias)}"

    case JoinClause(main, joins @ _*) =>
      printTableOrSubquery(main) + joins.map(printJoin).mkString
  }

  def printColumn(column: Column): String = {
    val table = column.table.map(table => s"${qualifiedName(table)}.").getOrElse("")
    s"$table${qualifiedName(column.name)}"
  }

  def printFunctionInvocation(function: FunctionInvocation): String = {
    val arguments = function.arguments match {
      case Some(Star) => "*"
      case Some(ArgumentsList(distinct, args @ _*)) =>
        val distinctString = if (distinct) "DISTINCT " else ""
        args.map(printExpression).mkString(distinctString, ", ", "")

      case _ => ""
    }

    s"${function.name}($arguments)"
  }

  def printBinaryOperatorApplication(application: BinaryOperatorApplication): String = {
    val operator = printBinaryOperator(application.operator)
    val left = printExpression(application.left)
    val right = printExpression(application.right)
    s"($left $operator $right)"
  }

  def printUnaryOperatorApplication(application: UnaryOperatorApplication): String = {
    val operator = printUnaryOperator(application.operator)
    val expression = printExpression(application.expression)
    s"$operator$expression"
  }

  def printBinaryOperator(operator: BinaryOperator): String = operator match {
    case Add => "+"
    case Subtract => "-"
    case Multiply => "*"
    case Divide => "/"
    case Modulus => "%"
    case Equals => "="
    case NotEquals => "!="
    case GreaterThan => ">"
    case GreaterThanOrEqualsTo => ">="
    case LessThan => "<"
    case LessThanOrEqualsTo => "<="
    case And => "AND"
    case Or => "OR"
    case In => "IN"
    case NotIn => "NOT IN"
    case Exists => "EXISTS"
    case NotExists => "NOT EXISTS"
    case Like => "LIKE"
    case NotLike => "NOT LIKE"
    case Concat => "||"
    case BitwiseAnd => "&"
    case BitwiseOr => "|"
    case ShiftLeft => "<<"
    case ShiftRight => ">>"
  }

  def printUnaryOperator(operator: UnaryOperator): String = operator match {
    case Not => "NOT "
    case BitwiseNot => "~"
  }

  def printExpressionList(list: ExpressionList): String = {
    list.expressions.map(printExpression).mkString("(", ", ", ")")
  }

  def printLiteral(literal: Literal): String = literal match {
    case LiteralString(value) => s"'$value'"
    case LiteralNumber(value) => s"$value"
    case LiteralNull => "NULL"
  }

  def printBetween(between: Between): String = {
    val expression = printExpression(between.expression)
    val lower = printExpression(between.lower)
    val upper = printExpression(between.upper)
    s"($expression BETWEEN $lower AND $upper)"
  }

  private def printExpression(expression: Expression): String = expression match {
    case literal: Literal => printLiteral(literal)
    case column: Column => printColumn(column)
    case function: FunctionInvocation => printFunctionInvocation(function)
    case list: ExpressionList => printExpressionList(list)
    case application: BinaryOperatorApplication => printBinaryOperatorApplication(application)
    case application: UnaryOperatorApplication => printUnaryOperatorApplication(application)
    case between: Between => printBetween(between)
  }

  private def printConditionClause(keyword: String, condition: Option[Expression]) = {
    condition.map { expression =>
      s"\n$keyword ${printExpression(expression)}"
    }.getOrElse("")
  }

  private def printGroupByClause(groupBy: Seq[Expression]): String = groupBy match {
    case expressions if expressions.nonEmpty =>
      groupBy.map(printExpression).mkString("\nGROUP BY ", ", ", "")

    case _ => ""
  }

  private def printJoin(join: Join): String = {
    val operator = join.operator match {
      case Default => ","
      case LeftJoin => "\nLEFT JOIN"
      case InnerJoin => "\nINNER JOIN"
      case CrossJoin => "\nCROSS JOIN"
    }

    val constraint = join.constraint match {
      case On(expression) => s" ON ${printExpression(expression)}"
      case Using(columns@_*) => columns.map(qualifiedName).mkString(" USING (", ", ", ")")
      case NoConstraint => ""
    }

    s"$operator ${printTableOrSubquery(join.table)}$constraint"
  }

  private def printAlias(alias: Option[String]): String =
    alias.map(alias => s" AS ${qualifiedName(alias)}").getOrElse("")

  implicit class StringOps(string: String) {
    def indent(amount: Int): String = {
      val margin = " " * amount
      string
        .lines
        .map(line => s"$margin$line")
        .mkString("\n")
    }
  }

}
