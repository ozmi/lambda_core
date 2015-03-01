package ozmi.lambda_core
package lib

import org.kiama.rewriting.Rewriter._

object Num extends TypeClass {
    
    object Zero extends NullaryOp ("Num.zero")
    
    object One extends NullaryOp ("Num.one")

    object Add extends BinaryOp ("Num.add") {
        override lazy val evalRules =
            rule[Expr] {
                case Add (Zero (), anyNum) => anyNum
                case Add (anyNum, Zero ()) => anyNum
            }
    }
    
    object Subtract extends BinaryOp ("Num.subtract") {
        override lazy val evalRules =
            rule[Expr] {
                case Subtract (Zero (), anyNum) => anyNum
                case Subtract (anyNum, Zero ()) => anyNum
            }
    }
    
    object Multiply extends BinaryOp ("Num.multiply") {
        override lazy val evalRules =
            rule[Expr] {
                case Multiply (Zero (), anyNum) => Zero ()
                case Multiply (anyNum, Zero ()) => Zero ()
                case Multiply (One (), anyNum) => anyNum
                case Multiply (anyNum, One ()) => anyNum
            }
    }
    
    val operators: Seq[Operator] = Seq (Add, Subtract, Multiply)
    
}