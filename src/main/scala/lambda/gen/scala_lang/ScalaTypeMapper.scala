package lambda.gen.scala_lang

import scala.collection.immutable.Seq
import scala.meta.{Defn, _}
import lambda.gen.TypeContext
import lambda.lang.Data.Type._
import lambda.lang.Exp._
import lambda.lang._

object ScalaTypeMapper extends scala.App {

    def toStatSeq (context : TypeContext) : Seq [Stat] = {
        context.thisType match {
            case module : Module =>
                val members =
                    for ((childTypeName, childType) <- module.childTypes.toVector) yield {
                        toStatSeq (context.descend (childTypeName, childType))
                    }
                if (context.isRoot) {
                    members.flatten
                } else {
                    Seq (
                        q"package ${Term.Name (context.thisName.snake_case)} { ..${members.flatten} }"
                    )
                }
            case record : Record =>
                if (record.fields.isEmpty) {
                    Seq (
                        q"case object ${Term.Name (context.thisName.TitleCase)}"
                    )
                } else {
                    val params =
                        for ((fieldName, fieldType) <- record.fields.toList)
                            yield param"${Term.Name (fieldName.camelCase)}: ${toType (context, fieldType)}"
                    Seq (
                        q"case class ${Type.Name (context.thisName.TitleCase)} (..$params)"
                    )
                }
            case taggedUnion : TaggedUnion =>
                val members =
                    for ((subTypeName, subType) <- taggedUnion.subTypes.toVector) yield {
                        toStatSeq (context.descend (subTypeName, subType))
                    }
                Seq (
                    q"sealed trait ${Type.Name (context.thisName.TitleCase)}",
                    q"object ${Term.Name (context.thisName.TitleCase)} { ..${members.flatten} }"
                )
            case tuple : Tuple =>
                Seq.empty
            case Lazy (exp) =>
                exp match {
                    case select : Select =>
                        Seq.empty
                    case apply : Apply =>
                        Seq.empty
                }
        }
    }

    def toType (context: TypeContext, targetType : Data.Type) : Type =
        targetType match {
            case tuple : Tuple =>
                val typeElems =
                    for (elem <- tuple.elems)
                        yield toType (context, elem)
                t"(..$typeElems)"
            case Lazy (exp) =>
                exp match {
                    case Const (tpe : Data.Type) =>
                        toType (context, tpe)
                    case typeSelect : Select =>
                        context.selectTypePath (typeSelect) match {
                            case Some (typePath) =>
                                val steps =
                                    for ((name, tpe) <- typeSelect.path zip typePath) yield tpe match {
                                        case _ : Module =>
                                            name.snake_case
                                        case _ =>
                                            name.TitleCase
                                    }
                                steps.mkString (".").parse [Type].get
                            case None =>
                                val packages = typeSelect.path.init map {_.snake_case}
                                (packages :+ typeSelect.path.last.TitleCase).mkString (".").parse [Type].get
                        }
                    case typeApply : Apply =>
                        val typeConst =
                            toType (context, typeApply.fun)
                        val typeArgs =
                            for (arg <- typeApply.args)
                                yield toType (context, arg)
                        t"$typeConst[..$typeArgs]"
                }
        }

    val tree = toStatSeq (TypeContext (Model.root))

    println (tree.head)

    def print (indent: String, tree : Tree) : Unit = {
        tree match {
            case d : Defn.Def =>
                println (d.name)
            case other =>
                println (indent + other.getClass.getName)
        }
        tree.children foreach {x => print (indent + "  ", x)}
    }

    val source = new java.io.File("src/main/scala/example/Calc.scala").parse[Source].get
    print ("", source.stats.head)

}
