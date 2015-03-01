package ozmi.lambda_core
package lib

import org.kiama.rewriting.Rewriter._

object Integer extends TypeInstance {

    import Eq._
    import Ord._
    import Num._
    
    override lazy val evalRules =
        rule[Expr] {
            // Eq
            case Equal (Literal (a : BigInt), Literal (b : BigInt)) if (a compare b) == 0 => Bool.True
            case Equal (Literal (a : BigInt), Literal (b : BigInt)) if (a compare b) != 0 => Bool.False
            // Ord
            case LessThan (Literal (a : BigInt), Literal (b : BigInt)) => Literal (a < b)
            case LessThanOrEqual (Literal (a : BigInt), Literal (b : BigInt)) => Literal (a <= b)
            case GreaterThan (Literal (a : BigInt), Literal (b : BigInt)) => Literal (a > b)
            case GreaterThanOrEqual (Literal (a : BigInt), Literal (b : BigInt)) => Literal (a >= b)
            // Num
            case Add (Literal (a : BigInt), Literal (b : BigInt)) => Literal (a + b)
            case Subtract (Literal (a : BigInt), Literal (b : BigInt)) => Literal (a - b)
            case Multiply (Literal (a : BigInt), Literal (b : BigInt)) => Literal (a * b)
        }
    
}