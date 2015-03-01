package ozmi.lambda_core
package lib

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object DecimalSpec extends Properties("Decimal") {
    
    import ozmi.lambda_core.lib.Num._
    
    lazy val saneBigDecimal : Gen[BigDecimal] =
        for {
            unscaledVal   <- Gen.choose(-1000, 1000)
            scale         <- Gen.choose(0, 100)
        } yield 
            BigDecimal(unscaledVal, scale)

    property("Add") = forAll(saneBigDecimal, saneBigDecimal) { (a: BigDecimal, b: BigDecimal) =>
        Library.eval (Add (Literal (a), Literal (b))) == Literal (a + b)
    }
    
    property("Subtract") = forAll(saneBigDecimal, saneBigDecimal) { (a: BigDecimal, b: BigDecimal) =>
        Library.eval (Subtract (Literal (a), Literal (b))) == Literal (a - b)
    }
    
    property("Multiply") = forAll(saneBigDecimal, saneBigDecimal) { (a: BigDecimal, b: BigDecimal) =>
        Library.eval (Multiply (Literal (a), Literal (b))) == Literal (a * b)
    }
    
}