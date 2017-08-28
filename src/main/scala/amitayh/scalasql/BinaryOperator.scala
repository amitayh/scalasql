package amitayh.scalasql

sealed trait BinaryOperator

sealed trait ArithmeticOperator extends BinaryOperator
case object Add extends ArithmeticOperator        // +
case object Subtract extends ArithmeticOperator   // -
case object Multiply extends ArithmeticOperator   // *
case object Divide extends ArithmeticOperator     // /
case object Modulus extends ArithmeticOperator    // %

sealed trait ComparisonOperator extends BinaryOperator
case object Equals extends BinaryOperator                 // =
case object NotEquals extends BinaryOperator              // !=
case object GreaterThan extends BinaryOperator            // >
case object GreaterThanOrEqualsTo extends BinaryOperator  // >=
case object LessThan extends BinaryOperator               // <
case object LessThanOrEqualsTo extends BinaryOperator     // <=

sealed trait LogicalOperator extends BinaryOperator
case object And extends LogicalOperator         // AND
case object Or extends LogicalOperator          // OR
case object In extends LogicalOperator          // IN             TODO maybe specialized type?
case object NotIn extends LogicalOperator       // NOT IN         TODO maybe specialized type?
case object Exists extends LogicalOperator      // EXISTS         TODO maybe specialized type?
case object NotExists extends LogicalOperator   // NOT EXISTS     TODO maybe specialized type?
case object Like extends LogicalOperator        // LIKE
case object NotLike extends LogicalOperator     // NOT LIKE
case object Concat extends LogicalOperator      // ||

sealed trait BitwiseOperator extends BinaryOperator
case object BitwiseAnd extends BitwiseOperator  // &
case object BitwiseOr extends BitwiseOperator   // |
case object ShiftLeft extends BitwiseOperator   // <<
case object ShiftRight extends BitwiseOperator  // >>
