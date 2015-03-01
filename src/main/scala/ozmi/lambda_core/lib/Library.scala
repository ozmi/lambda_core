package ozmi.lambda_core
package lib

import org.kiama.rewriting.Strategy
import org.kiama.rewriting.Rewriter._

object Library {

    val typeClasses : Seq[TypeClass] = Seq (Eq, Num, Ord, Row)
    val typeInstances : Seq[TypeInstance] = Seq (Bool, Coll, Decimal, Integer)
    
    lazy val evalRules : Strategy = 
        reduce ((typeClasses map {_.evalRules}) ++ (typeInstances map {_.evalRules}) reduceLeft {_ + _})
        
    def eval (expr : Expr) : Expr =
        rewrite (Library.evalRules) (expr)
    
}