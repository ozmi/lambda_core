package ozmi.lambda_core
package lib

/**
 * Created by attila on 3/1/2015.
 */
abstract class ProductRow (fieldNames : Seq[Id]) extends TypedRow {

    self : Product =>

    override def hasField (fieldName : Id) : Boolean =
        fieldNames contains fieldName

    override def get (fieldName : Id) : Expr =
        Literal (self.productElement(fieldNames indexOf fieldName))

}
