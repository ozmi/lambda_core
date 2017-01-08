package lambda.gen.scala_lang

import scala.collection.immutable.Seq
import scala.meta.Stat
import lambda.gen.TypeContext
import lambda.lang.TypeExp.TypeConst.{ ModuleType, RecordType, TaggedUnionType, TupleType }
import lambda.lang.TypeExp.{ TypeApply, TypeConst, TypeSelect }

abstract class TypeMapper {

    final def toDefn (context : TypeContext) : Seq [Stat] = {
        context.thisType match {
            case t : TypeConst =>
                t match {
                    case t : ModuleType =>
                        toDefn (context, t)
                    case t : RecordType =>
                        toDefn (context, t)
                    case t : TaggedUnionType =>
                        toDefn (context, t)
                    case t : TupleType =>
                        toDefn (context, t)
                }
            case t : TypeSelect =>
                toDefn (context, t)
            case t : TypeApply =>
                toDefn (context, t)
        }
    }

    def toDefn (context : TypeContext, thisType : ModuleType) : Seq [Stat]
    def toDefn (context : TypeContext, thisType : RecordType) : Seq [Stat]
    def toDefn (context : TypeContext, thisType : TaggedUnionType) : Seq [Stat]
    def toDefn (context : TypeContext, thisType : TupleType) : Seq [Stat]
    def toDefn (context : TypeContext, thisType : TypeSelect) : Seq [Stat]
    def toDefn (context : TypeContext, thisType : TypeApply) : Seq [Stat]


}
