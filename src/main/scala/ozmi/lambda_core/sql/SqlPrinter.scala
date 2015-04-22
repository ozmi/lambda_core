package ozmi.lambda_core
package sql

import org.kiama.output.PrettyExpression

object SqlPrinter extends org.kiama.output.ParenPrettyPrinter {

    def print (sqlExpr : SqlExpr) : String =
        sqlExpr match {
            case col : ColumnExpr => super.pretty (toParenDoc (col) )
            case rel : Relation => super.pretty (show (rel) )
        }

    def show (rel : Relation) : Doc =
        rel match {
            case ObjectRef (Some (schemaName), objectName) =>
                schemaName <> dot <> objectName
            case ObjectRef (None, objectName) =>
                objectName
            case Rename (baseRel, newName) =>
                show (baseRel) <+> "AS" <+> newName
            case Select (baseRel, selectors) =>
                "SELECT" <+> ssep (selectors.toList map show, ", ") <+> "FROM" <+> show (baseRel)
            case Where (baseRel, predicate) =>
                show (baseRel) <+> "WHERE" <+> toParenDoc (predicate)
            case Join (baseRel, joinType, joinedRel, joinPredicate) =>
                ???
        }

    def show (sel : Selector) : Doc =
        sel match {
            case SingleColumnSelector (expr, Some (alias)) =>
                toParenDoc (expr) <+> "AS" <+> alias
            case SingleColumnSelector (expr, None) =>
                toParenDoc (expr)
        }

    override def toParenDoc (expr : PrettyExpression) : Doc =
        expr match {
            case ColumnRef (Some (objectRef), columnRef) =>
                objectRef <> dot <> columnRef
            case ColumnRef (None, columnRef) =>
                columnRef
            case UnaryOp (op, arg) =>
                op <+> toParenDoc (arg)
            case bo : BinaryOp =>
                super.toParenDoc (bo)
            case lit : Literal =>
                show (lit)
        }

    def show (expr : Literal) : Doc =
        expr match {
            case IntegerLit (value) => value.toString
            case StringLit (value)  => "'" + value + "'"
            case DecimalLit (value) => value.bigDecimal.toPlainString
        }

}