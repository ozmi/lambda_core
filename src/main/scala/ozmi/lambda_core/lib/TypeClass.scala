package ozmi.lambda_core.lib

import org.kiama.rewriting.Strategy

trait TypeClass extends EvalRules {

    def operators : Seq[Operator]
    
    lazy val evalRules : Strategy = 
        operators map {
            _.evalRules
        } reduceLeft {
            _ + _
        }
    
}