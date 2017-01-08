package lambda.lang

/* This is a generated file */

import scala.collection.immutable.Seq

sealed trait TypeExp

object TypeExp {

    sealed trait TypeConst extends TypeExp

    object TypeConst {

        case class ModuleType (childTypes : Map [Name, TypeExp]) extends TypeConst
        case class RecordType (fields : Seq [(Name, TypeExp)]) extends TypeConst
        case class TaggedUnionType (subTypes : Map [Name, TypeExp]) extends TypeConst
        case class TupleType (fields : Seq [TypeExp]) extends TypeConst

    }

    case class TypeSelect (scope : TypeScope, path : Seq [Name]) extends TypeExp

    case class TypeApply (constructor : TypeExp, args : Seq [TypeExp]) extends TypeExp

}