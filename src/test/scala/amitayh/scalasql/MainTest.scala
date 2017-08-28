package amitayh.scalasql

import org.specs2.mutable.Specification

class MainTest extends Specification {

  "It" should {
    "work" in {
      val select = Select(
        columns = Vector(
          ExpressionColumn(Column("country")),
          ExpressionColumn(
            FunctionInvocation("COUNT", Some(Star)),
            alias = Some("cnt"))),
        from = JoinClause(
          Table("clicks", alias = Some("c")),
          Join(
            LeftJoin,
            Table("page_views", alias = Some("pv")),
            On(
              BinaryOperatorApplication(
                Equals,
                Column("id", Some("c")),
                Column("id", Some("pv")))))),
        where = Some(
          BinaryOperatorApplication(
            And,
            Between(Column("age"), LiteralNumber(20), LiteralNumber(40)),
            BinaryOperatorApplication(
              In,
              Column("language"),
              ExpressionList(LiteralString("EN"), LiteralString("DE"))))),
        groupBy = Vector(Column("country"))
      )

      ok
    }
  }

}
