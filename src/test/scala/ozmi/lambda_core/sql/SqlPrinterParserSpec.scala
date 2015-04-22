package ozmi.lambda_core.sql

import org.scalacheck.Prop._
import org.scalacheck.Properties

/**
 * Created by attila on 4/21/2015.
 */
class SqlPrinterParserSpec extends Properties("SqlPrinterParser") {

    val ab = ArbitrarySqlExpr.binaryOp (3)

    property("PrintParse") = forAll (ab) { (sqlExpr : ColumnExpr) =>
        val sqlString = SqlPrinter.print (sqlExpr)
        val Left (sqlExpr2) = SqlParser.parseColumnExpr (sqlString)
        ("sqlExpr2 = " + sqlExpr2) |: sqlExpr == sqlExpr2
    }

}
