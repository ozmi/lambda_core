package ozmi.lambda_core
package lib

import org.kiama.rewriting.Rewriter._

object Decimal extends TypeInstance {

    import Eq._
    import Ord._
    import Num._
 
    val TypeDesc = NamedType ("Decimal")
    val Type = TypeLiteral (TypeDesc)
    
    object DecimalType extends SubtypeOf (TypeDesc)
    
    override lazy val evalRules =
        rule[Expr] {
            // Eq
            case Equal (Literal (a : BigDecimal), Literal (b : BigDecimal)) if (a compare b) == 0 => Bool.True
            case Equal (Literal (a : BigDecimal), Literal (b : BigDecimal)) if (a compare b) != 0 => Bool.False
            // Ord
            case LessThan (Literal (a : BigDecimal), Literal (b : BigDecimal)) => Literal (a < b)
            case LessThanOrEqual (Literal (a : BigDecimal), Literal (b : BigDecimal)) => Literal (a <= b)
            case GreaterThan (Literal (a : BigDecimal), Literal (b : BigDecimal)) => Literal (a > b)
            case GreaterThanOrEqual (Literal (a : BigDecimal), Literal (b : BigDecimal)) => Literal (a >= b)
            // Num
            case Add (Literal (a : BigDecimal), Literal (b : BigDecimal)) => Literal (a + b)
            case Subtract (Literal (a : BigDecimal), Literal (b : BigDecimal)) => Literal (a - b)
            case Multiply (Literal (a : BigDecimal), Literal (b : BigDecimal)) => Literal (a * b)
        }
    
    val typeRules =
        rule[Expr] {
            // Eq
            case Equal (DecimalType (a), DecimalType(b)) => Bool.Type
            // Ord
            case LessThan (DecimalType (a), DecimalType (b)) => Bool.Type
            case LessThanOrEqual (DecimalType (a), DecimalType (b)) => Bool.Type
            case GreaterThan (DecimalType (a), DecimalType (b)) => Bool.Type
            case GreaterThanOrEqual (DecimalType (a), DecimalType (b)) => Bool.Type
            // Num
            case Add (DecimalType (a), DecimalType (b)) => DecimalType (TypeDesc)
            case Subtract (DecimalType (a), DecimalType (b)) => DecimalType (TypeDesc)
            case Multiply (DecimalType (a), DecimalType (b)) => DecimalType (TypeDesc)
        }
    
}