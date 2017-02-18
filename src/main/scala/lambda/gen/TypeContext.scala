package lambda.gen

import lambda.lang.Data.Type
import lambda.lang.Exp.{RootScope, Select}
import lambda.lang.Name

case class TypeContext (
    rootType : Type,
    namePath : Array [Name],
    typePath : Array [Type]
) {

    def isRoot : Boolean =
        namePath.isEmpty

    def descend (nextName : Name, nextType : Type) =
        TypeContext (rootType, namePath :+ nextName, typePath :+ nextType)

    def thisName : Name =
        namePath.lastOption getOrElse sys.error ("Invoked thisName on the root TypeContext!")

    def thisType : Type =
        typePath.last

    def selectTypePath (typeSelect : Select) : Option [Array [Type]] = {
        typeSelect.scope match {
            case RootScope =>
                TypeSearch.selectTypePath (rootType, typeSelect.path.toArray)
        }
    }

}

object TypeContext {

    def apply (rootType : Type) : TypeContext =
        TypeContext (rootType, Array.empty, Array (rootType))

}
