package lambda.lang


sealed trait Data

object Data {

    sealed trait Type extends Data

    object Type {

        case class Module (childTypes : Map [Name, Type]) extends Type
        case class Record (fields : Map [Name, Type]) extends Type
        case class TaggedUnion (subTypes : Map [Name, Type]) extends Type
        case class Tuple (elems : Vector [Type]) extends Type

        case class UnitType (value : Value) extends Type

        case class Lazy (exp : Exp) extends Type

    }

    sealed trait Value extends Data

    object Value {

        case class BooleanValue (value : Boolean) extends Value
        case class DecimalValue (value : BigDecimal) extends Value
        case class StringValue (value : String) extends Value

        case class ArrayValue (value : Vector [Value]) extends Value
        case class ObjectValue (fields : Map [Name, Value]) extends Value

        case class Lazy (exp : Exp) extends Value

    }

}
