package amitayh.scalasql

import scala.collection.immutable.Seq

case class Select(from: TableOrSubquery,
                  columns: Seq[ResultColumn] = Vector(All),
                  where: Option[Expression] = None,
                  groupBy: Seq[Expression] = Vector.empty,
                  having: Option[Expression] = None,
                  limit: Option[Expression] = None,
                  offset: Option[Expression] = None) {

  assert(columns.nonEmpty, "Result columns must contain at least one column")

  def withColumn(column: String): Select = withColumn(getColumn(column), None)

  def withColumn(column: String, alias: String): Select = withColumn(getColumn(column), Some(alias))

  def withColumn(expression: Expression): Select = withColumn(expression, None)

  def withColumn(expression: Expression, alias: String): Select = withColumn(expression, Some(alias))

  def withWhere(column: String, operator: BinaryOperator, value: Expression): Select = {
    val newCondition = BinaryOperatorApplication(operator, getColumn(column), value)
    val combinedConditions = where.map(BinaryOperatorApplication(And, _, newCondition))
    copy(where = combinedConditions orElse Some(newCondition))
  }

  def withGroupBy(column: String): Select = copy(groupBy = groupBy :+ getColumn(column))

  def withHaving(column: String, operator: BinaryOperator, value: Expression): Select = {
    val condition = BinaryOperatorApplication(operator, getColumn(column), value)
    copy(having = Some(condition))
  }

  private def withColumn(expression: Expression, alias: Option[String]): Select = {
    val column = ExpressionColumn(expression, alias)
    columns match {
      case Seq(All) => copy(columns = Vector(column))
      case _ => copy(columns = columns :+ column)
    }
  }

  private def getColumn(column: String): Column = column.split('.') match {
    case Array(table, name) => Column(name, Some(table))
    case _ => Column(column)
  }

}

object Select {
  def from(table: String): Select = Select(from = Table(table))
}

sealed trait ResultColumn

case object All extends ResultColumn

case class ExpressionColumn(expression: Expression,
                            alias: Option[String] = None) extends ResultColumn

sealed trait TableOrSubquery

case class JoinClause(main: TableOrSubquery, joins: Join*) extends TableOrSubquery

case class Join(operator: JoinOperator, table: TableOrSubquery, constraint: JoinConstraint)

sealed trait JoinConstraint
case object NoConstraint extends JoinConstraint
case class On(expression: Expression) extends JoinConstraint
case class Using(columns: String*) extends JoinConstraint {
  assert(columns.nonEmpty, "Columns list must contain at least one column name")
}

case class Table(name: String,
                 schema: Option[String] = None,
                 alias: Option[String] = None) extends TableOrSubquery
