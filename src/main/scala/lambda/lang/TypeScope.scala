package lambda.lang

sealed trait TypeScope

object TypeScope {

    case object RootModule extends TypeScope

}