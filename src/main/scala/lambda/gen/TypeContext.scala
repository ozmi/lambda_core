package lambda.gen

import lambda.lang.TypeExp.TypeSelect
import lambda.lang.TypeScope.RootModule
import lambda.lang.{ Name, TypeExp }

case class TypeContext (
    rootType : TypeExp,
    namePath : Array [Name],
    typePath : Array [TypeExp]
) {

    def isRoot : Boolean =
        namePath.isEmpty

    def descend (nextName : Name, nextType : TypeExp) =
        TypeContext (rootType, namePath :+ nextName, typePath :+ nextType)

    def thisName : Name =
        namePath.lastOption getOrElse sys.error ("Invoked thisName on the root TypeContext!")

    def thisType : TypeExp =
        typePath.last

    def selectTypePath (typeSelect : TypeSelect) : Option [Array [TypeExp]] = {
        typeSelect.scope match {
            case RootModule =>
                TypeSearch.selectTypePath (rootType, typeSelect.path.toArray)
        }
    }

}

object TypeContext {

    def apply (rootType : TypeExp) : TypeContext =
        TypeContext (rootType, Array.empty, Array (rootType))

}
