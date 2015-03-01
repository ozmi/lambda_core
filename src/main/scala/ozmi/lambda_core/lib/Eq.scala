package ozmi.lambda_core
package lib

import org.kiama.rewriting.Rewriter._

object Eq extends TypeClass {

    object Equal extends BinaryOp ("Eq.eq") {
        override lazy val evalRules =
            rule[Expr] {
                case Equal (a, b) if a == b  => Bool.True
                case Equal (a, b)            => Bool.False
            }
    }
    
    val operators: Seq[Operator] = Seq (Equal)
    
}