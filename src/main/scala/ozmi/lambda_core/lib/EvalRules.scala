package ozmi.lambda_core.lib

import org.kiama.rewriting.Strategy

trait EvalRules {

    def evalRules: Strategy
    
}