package lambda

import lambda.lang.Data.Type
import lambda.lang.Data.Type._
import lambda.lang.Exp._

package object lang {

    implicit def symbolToName (symbol : Symbol) =
        Name.parse.snake_case (symbol.name)

    implicit def expToType (exp : Exp) : Type =
        Type.Lazy (exp)

    implicit def typeToExp (tpe : Type) : Exp =
        Const (tpe)

    def module (subTypes : (Name, Type)*) =
        Module (subTypes.toMap)

    def record (fields : (Name, Type)*) =
        Record (fields.toMap)

    def taggedUnion (subTypes : (Name, Type)*) =
        TaggedUnion (subTypes.toMap)

    def tuple (elems : Type*) =
        Tuple (elems.toVector)

    def ref (path : Symbol*) =
        Select (RootScope, path.toVector map symbolToName)

    def apply (constructor : Exp, args : Exp*) =
        Apply (constructor, args.toVector)

    object BranchType {

        def unapply (typeExp : Type) : Option [Map [Name, Type]] = {
            typeExp match {
                case Module (children) =>
                    Some (children)
                case Record (fields) =>
                    Some (fields)
                case TaggedUnion (subtypes) =>
                    Some (subtypes)
                case Tuple (elems) =>
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
