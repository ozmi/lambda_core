package ozmi.lambda_core.sql

import org.kiama.util.ParserUtilities

/**
 * Created by attila on 4/20/2015.
 */
object SqlParser  extends ParserUtilities {

    def keyword = keywords ("""[^a-zA-Z0-9_]""".r, List ("SELECT"))

    // *** ColumnExpr ***
    def literal : PackratParser[Literal] =
        """\d+""".r ^^ {
            case string => IntegerLit (BigInt (string))
        }

    def ident : PackratParser[String] =
        not (keyword) ~> """\w+""".r

    def columnRef : PackratParser[ColumnRef] =
        opt (ident <~ ".") ~ ident ^^ {
            case objectName ~ columnName => ColumnRef (objectName, columnName)
        }

    def parensColumnExpr : PackratParser[ColumnExpr] = "(" ~> columnExpr <~ ")"

    def unaryMinus : PackratParser[ColumnExpr] = "-" ~> term ^^ { UnaryOp("-", _) }

    def term = literal | columnRef | parensColumnExpr | unaryMinus

    def binaryOp (level:Int) : PackratParser[((ColumnExpr, ColumnExpr) => ColumnExpr)] = {
        level match {
            case 1 =>
                "+" ^^^ { (a:ColumnExpr, b:ColumnExpr) => BinaryOp("+", a,b) } |
                "-" ^^^ { (a:ColumnExpr, b:ColumnExpr) => BinaryOp("-", a,b) }
            case 2 =>
                "*" ^^^ { (a:ColumnExpr, b:ColumnExpr) => BinaryOp("*", a,b) } |
                "/" ^^^ { (a:ColumnExpr, b:ColumnExpr) => BinaryOp("/", a,b) }
            case _ => throw new RuntimeException("bad precedence level "+level)
        }
    }
    val minPrec = 1
    val maxPrec = 2

    def binary (level : Int) : PackratParser[ColumnExpr] =
        if (level > maxPrec) term
        else binary (level+1) * binaryOp (level)

    def columnExpr = binary (minPrec) | term

    def parseColumnExpr (input : String) : Either[SqlExpr, String] = {
        parseString (columnExpr, input)
    }

    def objectRef : PackratParser[ObjectRef] =
        opt (ident <~ ".") ~ ident ^^ {
            case schemaName ~ objectName => ObjectRef (schemaName, objectName)
        }

    def singleColumnSelector : PackratParser[SingleColumnSelector] =
        columnExpr ~ opt (opt ("AS") ~> ident) ^^ {
            case columnExpr ~ alias => SingleColumnSelector (columnExpr, alias)
        }

    def allColumnsSelector : PackratParser[AllColumnsSelector] =
        opt (ident <~ ".") <~ "*" ^^ {
            case objectName => AllColumnsSelector (objectName)
        }

    def selector = singleColumnSelector | allColumnsSelector

    def select : PackratParser[Select] =
        "SELECT" ~> rep1sep (selector, ",") ~ ("FROM" ~> relation) ^^ {
            case selectors ~ baseRel => Select (baseRel, selectors)
        }

    def relation : PackratParser[Relation] =
        objectRef

    def parseRelationExpr (input : String) : Either[SqlExpr, String] = {
        parseString (select, input)
    }

}
