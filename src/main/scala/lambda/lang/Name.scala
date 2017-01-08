package lambda.lang

case class Name (words : Vector [String]) {

    def -> [T] (other : T) : (Name, T) =
        (this, other)

    def camelCase : String =
        words.head + (words.tail map {_.capitalize}).mkString

    def TitleCase : String =
        camelCase.capitalize

    def snake_case : String =
        words mkString "_"

    override def toString =
        snake_case

}

object Name {

    object parse {

        def camelCase (string : String) =
            Name (string.split ("(?<!(^|[A-Z]))(?=[A-Z])|(?<!^)(?=[A-Z][a-z])").toVector map (_.toLowerCase))

        def TitleCase (string : String) =
            camelCase (string)

        def snake_case (string : String) =
            Name (string.split ("\\_").toVector map (_.toLowerCase))

    }

}
