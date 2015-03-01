package ozmi.lambda_core

import org.kiama.rewriting.Strategy
import org.kiama.rewriting.Rewriter._

package object lib {
    
    abstract class Operator (val symbol : Id) extends EvalRules {
        def evalRules: Strategy = fail
    }
    
    abstract class NullaryOp (symbol : String) extends Operator (symbol) {
        
        def apply () : Expr = 
            Lookup (symbol)
        
        def unapply (expr : Expr) : Boolean = expr match {
            case Lookup (`symbol`) => true
            case _                 => false
        }
        
    }
    
    abstract class UnaryOp (symbol : Id) extends Operator (symbol) {
        
        def apply (arg : Expr) : Expr = 
            Apply (Lookup (symbol), arg)
        
        def unapply (expr : Expr) : Option[Expr] = Some (expr) collect {
            case Apply (Lookup (`symbol`), arg) => arg
        }
        
    }
    
    abstract class BinaryOp (symbol : String) extends Operator (symbol) {
        
        def apply (arg1 : Expr, arg2 : Expr) : Expr =
            Apply (Lookup (symbol), arg1, arg2)
        
        def unapply (expr : Expr) : Option[(Expr, Expr)] = Some (expr) collect {
            case Apply (Lookup (`symbol`), arg1, arg2) => (arg1, arg2)
        }
        
    }
    
    abstract class MultiaryOp (symbol : Id) extends Operator (symbol) {
        
        def apply (exprs : Expr*) : Expr =
            Apply (Lookup (symbol) +: exprs : _*)
        
        def unapplySeq (expr : Expr) : Option[Seq[Expr]] = Some (expr) collect {
            case Apply (Lookup (fun), args @ _*) => args
        }
        
    }
    
}