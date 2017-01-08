package lambda.gen

import lambda.lang.{ BranchType, Name, TypeExp }

object TypeSearch {

    def selectTypePath (rootType : TypeExp, path : Array [Name]) : Option [Array [TypeExp]] = {
        def inner (currentType : TypeExp, remainingPath : Array [Name], resultSoFar : Array [TypeExp]) : Option [Array [TypeExp]] = {
            if (remainingPath.isEmpty) {
                Some (resultSoFar)
            } else {
                currentType match {
                    case BranchType (lookup) =>
                        lookup.get (remainingPath.head) match {
                            case Some (nextType) =>
                                inner (nextType, remainingPath.tail, resultSoFar :+ nextType)
                            case None =>
                                None
                        }
                    case _ =>
                        None
                }
            }
        }
        inner (rootType, path, Array.empty)
    }

}
