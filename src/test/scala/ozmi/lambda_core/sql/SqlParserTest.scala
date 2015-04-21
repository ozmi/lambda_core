package ozmi.lambda_core.sql

import org.scalatest.FunSuite

/**
 * Created by attila on 4/20/2015.
 */
class SqlParserTest extends FunSuite {

    test ("precedence 1") {
        assert (SqlParser.parseColumnExpr ("1 + 2 * 3") ===
            Left (BinaryOp ("+", IntegerLit (1), BinaryOp ("*", IntegerLit (2), IntegerLit (3)))))
    }

    test ("precedence 2") {
        assert (SqlParser.parseColumnExpr ("(1 + 2) * 3") ===
            Left (BinaryOp ("*", BinaryOp ("+", IntegerLit (1), IntegerLit (2)), IntegerLit (3))))
    }

    test ("precedence 3") {
        assert (SqlParser.parseColumnExpr ("1 + (2 * 3)") ===
            Left (BinaryOp ("+", IntegerLit (1), BinaryOp ("*", IntegerLit (2), IntegerLit (3)))))
    }

    test ("precedence 4") {
        assert (SqlParser.parseColumnExpr ("1 + -2 * 3") ===
            Left (BinaryOp ("+", IntegerLit (1), BinaryOp ("*", UnaryOp ("-", IntegerLit (2)), IntegerLit (3)))))
    }

    test ("precedence 5") {
        assert (SqlParser.parseColumnExpr ("1 + -(2 * 3)") ===
            Left (BinaryOp ("+", IntegerLit (1), UnaryOp ("-", BinaryOp ("*", IntegerLit (2), IntegerLit (3))))))
    }

    test ("column ref") {
        assert (SqlParser.parseColumnExpr ("tbl1.col2 + col3") ===
            Left (BinaryOp ("+", ColumnRef (Some ("tbl1"), "col2"), ColumnRef (None, "col3"))))
    }

    test ("select 1") {
        assert (SqlParser.parseRelationExpr ("SELECT tbl1.col2 + col3, *, tbl1.*, col4 AS test FROM test_table") ===
            Left (Select (ObjectRef (None, "test_table"),
                Seq (
                    SingleColumnSelector (BinaryOp ("+", ColumnRef (Some ("tbl1"), "col2"), ColumnRef (None, "col3")), None),
                    AllColumnsSelector (None),
                    AllColumnsSelector (Some ("tbl1")),
                    SingleColumnSelector (ColumnRef (None, "col4"), Some ("test"))
                ))))
    }
}
