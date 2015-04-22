package ozmi.lambda_core.sql

import org.scalacheck.Gen

/**
 * Created by attila on 4/21/2015.
 */
object ArbitrarySqlExpr {

    lazy val saneBigDecimal : Gen[BigDecimal] =
        for {
            unscaledVal   <- Gen.choose (0, 1000) // use only positive tests to avoid confusion with unary -
            scale         <- Gen.choose (1, 10) // make sure that decimals have digits after the decimal point
        } yield
            BigDecimal (unscaledVal, scale)

    lazy val literal : Gen[ColumnExpr] = Gen.oneOf (integerLit, decimalLit, stringLit)
    lazy val integerLit = for (i <- Gen.choose(0, 10000)) yield IntegerLit (i) // use only positive tests to avoid confusion with unary -
    lazy val decimalLit = for (i <- saneBigDecimal) yield DecimalLit (i)
    lazy val stringLit  = for (i <- Gen.alphaStr) yield StringLit (i)

    def binaryOp (depth : Int) : Gen[ColumnExpr] = {
        val argGen =
            if (depth <= 0) literal
            else Gen.oneOf (literal, binaryOp (depth - 1))
        for {
            op <- Gen.oneOf ("+", "-", "*", "/")
            arg1 <- argGen
            arg2 <- argGen
        } yield BinaryOp (op, arg1, arg2)
    }

}
