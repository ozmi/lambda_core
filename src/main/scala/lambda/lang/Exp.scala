package lambda.lang

sealed trait Exp

object Exp {

    case class Const (data : Data) extends Exp
    case class Var (name : Name) extends Exp
    case class Apply (fun : Exp, args : Vector [Exp]) extends Exp
    case class Lambda (argNames : Vector [Name], body : Exp) extends Exp

    case class Select (scope : Exp, path : Vector [Name]) extends Exp
    case class Let (bindings : Vector [(Name, Exp)], in : Exp) extends Exp

    /** Lets the logic branch out based on the value of an expression. It can be used to model if-else
      * constructs, switch statements or
      *
      * @param from the expression that controls which branch gets selected during evaluation
      * @param branches (pattern, result) pairs, if [[from]] matches data then expression will be returned during
      *                 evaluation
      */
    case class Branch (from : Exp, branches : Vector [(Exp, Exp)]) extends Exp

    case object RootScope extends Exp

}