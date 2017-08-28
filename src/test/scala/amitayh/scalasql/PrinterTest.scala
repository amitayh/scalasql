package amitayh.scalasql

import amitayh.scalasql.Printer._
import org.specs2.mutable.Specification
import org.specs2.specification.core.Fragments

class PrinterTest extends Specification {

  "qualified name" should {
    "surround name with quotes" in {
      qualifiedName("foo") must equalTo("\"foo\"")
    }
  }

  "print column" >> {
    "for simple column names" in {
      printColumn(Column("foo")) must equalTo("\"foo\"")
    }

    "with optional table name" in {
      printColumn(Column("foo", table = Some("bar"))) must equalTo("\"bar\".\"foo\"")
    }
  }

  "print function invocation" >> {
    "without arguments" in {
      printFunctionInvocation(FunctionInvocation("RANDOM")) must equalTo("RANDOM()")
    }

    "with star in arguments list" in {
      printFunctionInvocation(FunctionInvocation("COUNT", Some(Star))) must equalTo("COUNT(*)")
    }

    "with one argument in list" in {
      val function = FunctionInvocation("ABS", Some(ArgumentsList(LiteralNumber(-1))))

      printFunctionInvocation(function) must equalTo("ABS(-1)")
    }

    "with multiple arguments in list" in {
      val function = FunctionInvocation(
        "SUBSTR",
        Some(
          ArgumentsList(
            LiteralString("foo"),
            LiteralNumber(2))))

      printFunctionInvocation(function) must equalTo("SUBSTR('foo', 2)")
    }

    "with distinct keyword" in {
      val function = FunctionInvocation(
        "COUNT",
        Some(
          ArgumentsList(
            distinct = true,
            Column("name"))))

      printFunctionInvocation(function) must equalTo("COUNT(DISTINCT \"name\")")
    }

    "fail for empty arguments list" in {
      // TODO move test?
      FunctionInvocation("COUNT", Some(ArgumentsList())) must throwA[AssertionError]
    }
  }

  "print binary operator" >> {
    val examples: Seq[(BinaryOperator, String)] = Seq(
      Add -> "+",
      Subtract -> "-",
      Multiply -> "*",
      Divide -> "/",
      Modulus -> "%",
      Equals -> "=",
      NotEquals -> "!=",
      GreaterThan -> ">",
      GreaterThanOrEqualsTo -> ">=",
      LessThan -> "<",
      LessThanOrEqualsTo -> "<=",
      And -> "AND",
      Or -> "OR",
      In -> "IN",
      NotIn -> "NOT IN",
      Exists -> "EXISTS",
      NotExists -> "NOT EXISTS",
      Like -> "LIKE",
      NotLike -> "NOT LIKE",
      Concat -> "||",
      BitwiseAnd -> "&",
      BitwiseOr -> "|",
      ShiftLeft -> "<<",
      ShiftRight -> ">>"
    )

    Fragments.foreach(examples) {
      case (operator, expected) =>
        s"for $operator" in {
          printBinaryOperator(operator) must equalTo(expected)
        }
    }
  }

  "print unary operator" >> {
    val examples: Seq[(UnaryOperator, String)] = Seq(Not -> "NOT ", BitwiseNot -> "~")

    Fragments.foreach(examples) {
      case (operator, expected) =>
        s"for $operator" in {
          printUnaryOperator(operator) must equalTo(expected)
        }
    }
  }

  "print binary operator application" in {
    val application = BinaryOperatorApplication(GreaterThan, Column("age"), LiteralNumber(20))

    printBinaryOperatorApplication(application) must equalTo("(\"age\" > 20)")
  }

  "print unary operator application" in {
    val application = UnaryOperatorApplication(Not, LiteralNull)

    printUnaryOperatorApplication(application) must equalTo("NOT NULL")
  }

  "print expression list" >> {
    "with one expression" in {
      val list = ExpressionList(LiteralNumber(1))

      printExpressionList(list) must equalTo("(1)")
    }

    "with multiple expressions" in {
      val list = ExpressionList(LiteralNumber(1), LiteralNumber(2))

      printExpressionList(list) must equalTo("(1, 2)")
    }

    "fail for empty list" in {
      // TODO move test?
      ExpressionList() must throwA[AssertionError]
    }
  }

  "print literal" >> {
    "string value" in {
      printLiteral(LiteralString("foo")) must equalTo("'foo'")
    }

    "numeric value" in {
      printLiteral(LiteralNumber(1.23)) must equalTo("1.23")
    }

    "null" in {
      printLiteral(LiteralNull) must equalTo("NULL")
    }
  }

  "print between" in {
    val between = Between(Column("age"), LiteralNumber(20), LiteralNumber(30))

    printBetween(between) must equalTo("(\"age\" BETWEEN 20 AND 30)")
  }

  "print result column" >> {
    "for star" in {
      printResultColumn(All) must equalTo("*")
    }

    "for expression without alias" in {
      printResultColumn(ExpressionColumn(LiteralNumber(1))) must equalTo("1")
    }

    "for expression with alias" in {
      val column = ExpressionColumn(LiteralNumber(1), alias = Some("one"))

      printResultColumn(column) must equalTo("1 AS \"one\"")
    }
  }

  "print table or subquery" in {
    "for simple table definition" in {
      printTableOrSubquery(Table("foo")) must equalTo("\"foo\"")
    }

    "for table with schema" in {
      val table = Table("foo", schema = Some("bar"))

      printTableOrSubquery(table) must equalTo("\"bar\".\"foo\"")
    }

    "for table with alias" in {
      val table = Table("foo", alias = Some("bar"))

      printTableOrSubquery(table) must equalTo("\"foo\" AS \"bar\"")
    }

    "for multiple table join" in {
      val join = JoinClause(Table("foo"), Join(Default, Table("bar"), NoConstraint))

      printTableOrSubquery(join) must equalTo("\"foo\", \"bar\"")
    }

    "for left join using one column" in {
      val join = JoinClause(Table("foo"), Join(LeftJoin, Table("bar"), Using("baz")))

      val expected =
        """
          |"foo"
          |LEFT JOIN "bar" USING ("baz")
        """.stripMargin.trim


      printTableOrSubquery(join) must equalTo(expected)
    }

    "for left join using multiple column" in {
      val join = JoinClause(Table("foo"), Join(LeftJoin, Table("bar"), Using("baz", "qux")))

      val expected =
        """
          |"foo"
          |LEFT JOIN "bar" USING ("baz", "qux")
        """.stripMargin.trim

      printTableOrSubquery(join) must equalTo(expected)
    }

    "for inner join on constraint" in {
      val join = JoinClause(
        Table("foo"),
        Join(
          InnerJoin,
          Table("bar"),
          On(
            BinaryOperatorApplication(
              Equals,
              Column("id", Some("foo")),
              Column("id", Some("bar"))))))

      val expected =
        """
          |"foo"
          |INNER JOIN "bar" ON ("foo"."id" = "bar"."id")
        """.stripMargin.trim

      printTableOrSubquery(join) must equalTo(expected)
    }
  }

  "print select" >> {
    "with all columns, no conditions" in {
      val select = Select.from("foo")

      val expected =
        """
          |SELECT
          |  *
          |FROM
          |  "foo"
        """.stripMargin.trim

      printSelect(select) must equalTo(expected)
    }

    "with two columns, no conditions" in {
      val select = Select.from("foo")
        .withColumn("foo.bar", "bar")
        .withColumn("foo.baz", "baz")

      val expected =
        """
          |SELECT
          |  "foo"."bar" AS "bar",
          |  "foo"."baz" AS "baz"
          |FROM
          |  "foo"
        """.stripMargin.trim

      printSelect(select) must equalTo(expected)
    }

    "with simple where clause" in {
      val select = Select.from("foo")
        .withWhere("bar", Equals, LiteralString("baz"))

      val expected =
        """
          |SELECT
          |  *
          |FROM
          |  "foo"
          |WHERE ("bar" = 'baz')
        """.stripMargin.trim

      printSelect(select) must equalTo(expected)
    }

    "with compound where clause" in {
      val select = Select.from("foo")
        .withWhere("bar", Equals, LiteralString("baz"))
        .withWhere("baz", In, ExpressionList(LiteralNumber(1), LiteralNumber(2)))

      val expected =
        """
          |SELECT
          |  *
          |FROM
          |  "foo"
          |WHERE (("bar" = 'baz') AND ("baz" IN (1, 2)))
        """.stripMargin.trim

      printSelect(select) must equalTo(expected)
    }

    "with group by clause" in {
      val select = Select.from("clicks")
        .withColumn("country")
        .withColumn(FunctionInvocation("COUNT", Some(Star)), alias = "cnt")
        .withGroupBy("country")

      val expected =
        """
          |SELECT
          |  "country",
          |  COUNT(*) AS "cnt"
          |FROM
          |  "clicks"
          |GROUP BY "country"
        """.stripMargin.trim

      printSelect(select) must equalTo(expected)
    }

    "with having clause" in {
      val select = Select.from("clicks")
        .withColumn("country")
        .withColumn(FunctionInvocation("COUNT", Some(Star)), alias = "cnt")
        .withGroupBy("country")
        .withHaving("cnt", GreaterThan, LiteralNumber(10))

      val expected =
        """
          |SELECT
          |  "country",
          |  COUNT(*) AS "cnt"
          |FROM
          |  "clicks"
          |GROUP BY "country"
          |HAVING ("cnt" > 10)
        """.stripMargin.trim

      printSelect(select) must equalTo(expected)
    }
  }

}
