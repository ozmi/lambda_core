package ozmi.lambda_core
package lib

import org.kiama.rewriting.Rewriter._

/**
 * Created by attila on 3/1/2015.
 */
object Row extends TypeClass {

    override val operators = Seq()

    override lazy val evalRules =
        rule[Expr] {
            case Apply (Lookup (fieldName), Literal (row : TypedRow)) if row hasField fieldName => row get fieldName
        }

}
