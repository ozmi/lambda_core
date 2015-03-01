package ozmi.lambda_core
package lib

import org.scalacheck.Properties
import org.scalacheck.Prop.forAll
import org.kiama.rewriting.Rewriter._
import org.kiama.util.ErrorEmitter
import org.scalacheck.Arbitrary
import java.math.MathContext
import org.scalacheck.Gen

object IntegerSpec extends Properties("Integer") {
    
    import Num._
    
    property("Add") = forAll { (a: BigInt, b: BigInt) =>
        Library.eval (Add (Literal (a), Literal (b))) == Literal (a + b)
    }
    
    property("Subtract") = forAll { (a: BigInt, b: BigInt) =>
        Library.eval (Subtract (Literal (a), Literal (b))) == Literal (a - b)
    }
    
    property("Multiply") = forAll { (a: BigInt, b: BigInt) =>
        Library.eval (Multiply (Literal (a), Literal (b))) == Literal (a * b)
    }
    
}