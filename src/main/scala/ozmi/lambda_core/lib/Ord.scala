package ozmi.lambda_core
package lib

object Ord extends TypeClass {

    object LessThan extends BinaryOp ("Ord.lt")
    
    object LessThanOrEqual extends BinaryOp ("Ord.lte")
    
    object GreaterThan extends BinaryOp ("Ord.gt")
    
    object GreaterThanOrEqual extends BinaryOp ("Ord.gte")
    
    val operators: Seq[Operator] = Seq (LessThan, LessThanOrEqual, GreaterThan, GreaterThanOrEqual)
    
}