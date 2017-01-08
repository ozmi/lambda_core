package lambda

import lambda.lang.TypeExp.TypeConst.{ ModuleType, RecordType, TaggedUnionType, TupleType }
import lambda.lang.TypeExp.{ TypeApply, TypeSelect }

package object lang {

    implicit def symbolToName (symbol : Symbol) =
        Name.parse.snake_case (symbol.name)

    def module (subTypes : (Name, TypeExp)*) =
        ModuleType (subTypes.toMap)

    def record (fields : (Name, TypeExp)*) =
        RecordType (fields.toVector)

    def taggedUnion (subTypes : (Name, TypeExp)*) =
        TaggedUnionType (subTypes.toMap)

    def tuple (elems : TypeExp*) =
        TupleType (elems.toVector)

    def ref (path : Symbol*) =
        TypeSelect (TypeScope.RootModule, path.toVector map symbolToName)

    def apply (constructor : TypeExp, args : TypeExp*) =
        TypeApply (constructor, args.toVector)

    object BranchType {

        def unapply (typeExp : TypeExp) : Option [Map [Name, TypeExp]] = {
            typeExp match {
                case ModuleType (children) =>
                    Some (children)
                case RecordType (fields) =>
                    Some (fields.toMap)
                case TaggedUnionType (subtypes) =>
                    Some (subtypes)
                case TupleType (elems) =>
                    val keys =
                        for (index <- 1 to elems.size) yield {
                            Name (Vector ("elem", index.toString))
                        }
                    Some ((keys zip elems).toMap)
                case _ =>
                    None
            }
        }

    }

}
