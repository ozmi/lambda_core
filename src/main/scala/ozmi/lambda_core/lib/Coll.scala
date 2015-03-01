package ozmi.lambda_core
package lib

import org.kiama.rewriting.Rewriter._
import org.kiama.rewriting.Strategy

object Coll extends TypeInstance {
    
    abstract class TransformOp (symbol : Id) extends Operator (symbol) {
        
        def apply (coll : Expr, fun : Lambda) : Expr =
            Apply (Lookup (symbol), coll, fun)
        
        def unapply (expr : Expr) : Option[(Expr, Lambda)] = Some(expr) collect {
            case Apply (Lookup (`symbol`), coll, fun: Lambda) => (coll, fun)
        }
        
    }

    object Filter extends TransformOp ("Coll.filter") {
        override lazy val evalRules =
            rule[Expr] {
                case Filter (Literal (coll : Traversable[_]), Lambda (Seq (itemVar), body)) =>
                    val newColl =
                        coll filter {item =>
                            val itemBody = replaceVariable (body, itemVar, item)
                            rewrite (Library.evalRules) (itemBody) match {
                                case Literal (v : Boolean) => v
                            }
                        }
                    Literal (newColl)
            }
    }
    
    object Map extends TransformOp ("Coll.map") {
        override lazy val evalRules =
            rule[Expr] {
                case Map (Literal (coll : Traversable[_]), Lambda (Seq (itemVar), body)) => 
                    val newColl =
                        coll map {item =>
                            val itemBody = replaceVariable (body, itemVar, item)
                            rewrite (Library.evalRules) (itemBody) match {
                                case Literal (v) => v
                            }
                        }
                    Literal (newColl)
            }
    }
    
    object Reduce extends TransformOp ("Coll.reduce") {
        override lazy val evalRules =
            rule[Expr] {
                case Reduce (Literal (coll : Traversable[Any]), Lambda (Seq (itemVarA, itemVarB), body)) => 
                    if (coll.isEmpty) {
                        Literal (null)
                    } else {
                        val result =
                            coll reduce {(itemA, itemB) =>
                                val itemBody = replaceVariable (replaceVariable (body, itemVarA, itemA), itemVarB, itemB)
                                rewrite (Library.evalRules) (itemBody) match {
                                    case Literal (v) => v
                                }
                            }
                        Literal (result)
                    }
            }
    }
    
    val operators: Seq[Operator] = Seq (Filter, Map, Reduce)
    
    def replaceVariable (body: Expr, varName: Id, varValue: Any): Expr = {
        val replaceRules =
            rule[Expr] {
                case Lookup (v) if v == varName => varValue match {
                    case other => Literal (other)
                }
            }
        rewrite (reduce (replaceRules)) (body)
    }
    
    override lazy val evalRules: Strategy =
        operators map {
            _.evalRules
        } reduceLeft {
            _ + _
        }
    
}