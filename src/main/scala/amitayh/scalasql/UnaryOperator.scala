package amitayh.scalasql

sealed trait UnaryOperator

case object Not extends UnaryOperator         // NOT
case object BitwiseNot extends UnaryOperator  // ~
