package amitayh.scalasql

sealed trait JoinOperator
case object Default extends JoinOperator
case object LeftJoin extends JoinOperator
case object InnerJoin extends JoinOperator
case object CrossJoin extends JoinOperator
