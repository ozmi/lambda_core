package lambda.lang

case class Path (names : Array [Name]) {

    def isRoot : Boolean =
        names.isEmpty

    def ascend : Option [Path] =
        if (isRoot) None
        else Some (Path (names.init))

    def / (nextName : Name) : Path =
        Path (names :+ nextName)

    def last : Name =
        names.last

}

object Path {

    def apply (name : Name) : Path =
        Path (Array (name))

}
