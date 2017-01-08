package lambda.gen.scala_lang

import scala.collection.immutable.Seq
import scala.meta._
import lambda.gen.TypeContext
import lambda.lang.TypeExp.{ TypeApply, TypeConst, TypeSelect }
import lambda.lang._
import lambda.lang.TypeExp.TypeConst.{ ModuleType, RecordType, TaggedUnionType, TupleType }

object ScalaTypeMapper extends TypeMapper with scala.App {

    override def toDefn (context : TypeContext, typeSelect : TypeSelect) : Seq [Stat] = {
        Seq.empty
    }

    def x (context : TypeContext, typeSelect : TypeSelect) : Type =
        context.selectTypePath (typeSelect) match {
            case Some (typePath) =>
                val steps =
                    for ((name, tpe) <- typeSelect.path zip typePath) yield tpe match {
                        case _ : ModuleType =>
                            name.snake_case
                        case _ =>
                            name.TitleCase
                    }
                steps.mkString (".").parse [Type].get
            case None =>
                val packages = typeSelect.path.init map {_.snake_case}
                (packages :+ typeSelect.path.last.TitleCase).mkString (".").parse [Type].get
        }

    override def toDefn (context : TypeContext, thisType : TypeApply) : Seq [Stat] = {
        Seq.empty
    }

    override def toDefn (context : TypeContext, thisType : ModuleType) : Seq [Stat] = {
        val members =
            for ((childTypeName, childType) <- thisType.childTypes.toVector) yield {
                toDefn (context.descend (childTypeName, childType))
            }
        if (context.isRoot) {
            members.flatten
        } else {
            Seq (
                q"package ${Term.Name (context.thisName.snake_case)} { ..${members.flatten} }"
            )
        }
    }

    override def toDefn (context : TypeContext, thisType : RecordType) : Seq [Stat] = {
        if (thisType.fields.isEmpty) {
            Seq (
                q"case object ${Term.Name (context.thisName.TitleCase)}"
            )
        } else {
            val params =
                for ((fieldName, fieldType) <- thisType.fields) yield fieldType match {
                    case ts : TypeSelect =>
                        param"${Term.Name (fieldName.camelCase)}: ${x (context, ts)}"
                    case _ =>
                        param"${Term.Name (fieldName.camelCase)}: String"
                }
            Seq (
                q"case class ${Type.Name (context.thisName.TitleCase)} (..$params)"
            )
        }
    }

    override def toDefn (context : TypeContext, thisType : TaggedUnionType) : Seq [Stat] = {
        val members =
            for ((subTypeName, subType) <- thisType.subTypes.toVector) yield {
                toDefn (context.descend (subTypeName, subType))
            }
        Seq (
            q"sealed trait ${Type.Name (context.thisName.TitleCase)}",
            q"object ${Term.Name (context.thisName.TitleCase)} { ..${members.flatten} }"
        )
    }

    override def toDefn (context : TypeContext, thisType : TupleType) : Seq[Stat] = ???

    val tree = toDefn (TypeContext (Model.root))

    println (tree.head)

    def print (stat : Stat) : Unit = {
        stat match {
            case pkg : Pkg =>
                println (pkg.ref)
                pkg.stats foreach print
            case other =>
                println (other)
        }
    }

//    val source = new java.io.File("src/main/scala/lambda/lang/TypeExp.scala").parse[Source].get
//    println (source.stats.head.getClass)

}
