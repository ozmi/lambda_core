package ozmi.lambda_core.lib

import ozmi.lambda_core._

/**
 * Created by attila on 3/1/2015.
 */
trait TypedRow {

    def hasField (fieldName : Id) : Boolean

    def get (fieldName : Id) : Expr

}
