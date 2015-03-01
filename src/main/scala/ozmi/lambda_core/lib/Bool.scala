package ozmi.lambda_core
package lib

import org.kiama.rewriting.Rewriter._

object Bool extends TypeInstance {

    val TypeDesc = NamedType ("Bool")
    val Type = TypeLiteral (TypeDesc)
    
    val True    = Literal (true)
    val False   = Literal (false)
    
    object Not extends UnaryOp ("Bool.not")
    
    object And extends BinaryOp ("Bool.and")
    
    object Or extends BinaryOp ("Bool.or")
    
    val operators: Seq[Operator] = Seq (Not, And, Or)
    
    override lazy val evalRules =
        rule[Expr] {
            case Not (True) => False
            case Not (False) => True
            case And (False, _) => False
            case And (_, False) => False
            case And (True, True) => True
            case Or (True, _) => True
            case Or (_, True) => True
            case Or (False, False) => False
        }
    
}