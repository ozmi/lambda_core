package lambda.gen

import lambda.lang.Data.Type
import lambda.lang.{BranchType, Name}

object TypeSearch {

    def selectTypePath (rootType : Type, path : Array [Name]) : Option [Array [Type]] = {
        def inner (currentType : Type, remainingPath : Array [Name], resultSoFar : Array [Type]) : Option [Array [Type]] = {
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
