package ozmi.lambda_core
package lib

import org.scalacheck.Prop.forAll
import org.scalacheck.Properties

object BoolSpec extends Properties("Bool") {
    
    import ozmi.lambda_core.lib.Bool._

    property("Not") = forAll { (a: Boolean) =>
        Library.eval (Not (Literal (a))) == Literal (!a)
    }
    
    property("And") = forAll { (a: Boolean, b: Boolean) =>
        Library.eval (And (Literal (a), Literal (b))) == Literal (a && b)
    }
    
    property("Or") = forAll { (a: Boolean, b: Boolean) =>
        Library.eval (Or (Literal (a), Literal (b))) == Literal (a || b)
    }
    
}