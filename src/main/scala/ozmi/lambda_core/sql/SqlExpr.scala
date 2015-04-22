package ozmi.lambda_core.sql

import org.kiama.output._

sealed abstract class SqlExpr extends PrettyExpression

sealed abstract class Relation extends SqlExpr
case class ObjectRef (schemaName : Option[String], objectName : String) extends Relation
case class Rename (baseRel : Relation, newName : String) extends Relation
case class Select (baseRel : Relation, selectors : Seq[Selector]) extends Relation
case class Where (baseRel : Relation, predicate : ColumnExpr) extends Relation
case class Join (baseRel : Relation, joinType : JoinType, joinedRel : Relation, joinPredicate : ColumnExpr) extends Relation

sealed trait Selector
case class SingleColumnSelector (expr : ColumnExpr, alias : Option[String]) extends Selector
case class AllColumnsSelector (objectAlias : Option[String]) extends Selector

sealed trait JoinType
case object InnerJoin extends JoinType

sealed trait ColumnExpr extends SqlExpr
case class ColumnRef (objectRef : Option[String], columnRef : String) extends ColumnExpr
case class UnaryOp (operator : String, arg : ColumnExpr) extends ColumnExpr
case class BinaryOp (operator : String, arg1 : ColumnExpr, arg2 : ColumnExpr) extends ColumnExpr with PrettyBinaryExpression {
    val fixity      = BinaryOp.fixity (operator)
    val priority    = BinaryOp.priority (operator)
    val left        = arg1
    val op          = operator
    val right       = arg2
}
object BinaryOp {
    def fixity (op : String) : Fixity =
        op match {
            case "+" | "-" | "*" | "/" => Infix (LeftAssoc)
        }

    def priority (op : String) : Int =
        // lower number is higher priority
        op match {
            case "+" | "-" => 2
            case "*" | "/" => 1
        }
}
case class ColFunCall (functionName : String, args : ColumnExpr*) extends ColumnExpr

sealed trait Literal extends ColumnExpr
case class IntegerLit (value : BigInt) extends Literal
case class StringLit (value : String) extends Literal
case class DecimalLit (value : BigDecimal) extends Literal

