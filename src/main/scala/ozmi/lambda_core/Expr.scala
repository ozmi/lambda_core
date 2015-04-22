package ozmi.lambda_core

import org.kiama.util.TreeNode

/** Base trait for expression tree nodes.
  *
  * Implements Kiama's TreeNode which is mostly used for attribution.
  */
sealed abstract class Expr extends TreeNode

trait Value extends Expr

/** Lookup is used to refer to any external resources in the execution context.
  *
  * Those resources could have different arities:
  * - nullary resources refer to static values, in other words they are constants
  * - unary resources may refer to
  *   - functions with one argument (such as `not` in `not(true)`) or
  *   - '''immutable properties''' (such as `birthDate` in `birthDate(attmih)` or `attmih.birthDate`)
  * - binary resources may refer to
  *   - functions with two arguments (such as `and` in `and(true, false)` or `true && false`) or
  *   - '''mutable properties''' (such as `weight` in `weight(attmih, now)` or `attmih.weight(now)`)
  * - ternary resources may refer to
  *   - functions with three arguments (the obvious example would be the ternary operator
  *     but since that's covered by the Branch node it should never be used in a lookup)
  *   - bitemporal properties
  * - and the list goes on ...
  *
  * @param id the id to look up from the context
  */
case class Lookup (id : Id) extends Expr with Value {
    override def toString () = id
}

/** Literal is conceptually the same as a Lookup in that it refers to resources.
  *  
  * The only purpose of differentiating literals is to avoid the unnecessary cost
  * of naming and resolving primitive values that can be easily represented by values
  * in the host language. 
  * 
  * The type of values allowed here might be more constrained in the future 
  * but for now it can be any values or object references.  
  */
case class Literal (value : Any) extends Expr with Value

case class TypeLiteral (typeDesc : TypeDesc) extends Expr

/** Apply can be used to invoke functions with concrete arguments.
  *
  * The first argument of apply should be an expression that returns a function.
  * In the simplest case this is a Lookup that refers to a function but it could
  * be any expression that returns a function.
  *
  * Functions with more than one arguments can be invoked by applying multiple times.
  * For example a function invocation with 3 arguments `f(a, b, c)` would look like this:
  * {{{
  * Apply (Apply (Apply (f, a), b), c)
  * }}}
  *
  * This makes it easy to partially apply any functions which makes composition easier.
  *
  * @param e1 the expression to apply to
  * @param e2 the expression that will be applied to [[e1]]
  */
case class Apply (exprs : Expr*) extends Expr {
    override def toString () = s"(${exprs mkString " "})"
}

/** Lambda represents a function body with a single bound variable.
  *
  * Similar to the Apply node Lambda can be applied multiple times to represent higher arity functions.
  * For example a lambda expression with 2 arguments `(a, b) => a + b` would look like this:
  * {{{
  * Lambda ("a",  Lambda ("b", Apply (Apply (+, Lookup ("a")), Lookup ("b"))))
  * }}}
  *
  * @param fun function that expects an expression and returns the body of this lambda
  */
case class Lambda (args : Seq[Id], body : Expr) extends Expr {
    override def toString () = (args map {arg => s"\u03BB${arg}."} mkString "") + body
}

/** A let expression allows you to assign sub-trees to ids making it easier to reuse them
  * in the nested expression.
  *
  * Declarations are mutually-recursive meaning that you can refer to ids from any expression
  * nested in the node including the right-hand sides of the declarations themselves regardless
  * of the order in which they are defined.
  *
  * The use of a Map is intentional to ensure id uniqueness and make it impossible to rely on
  * ordering.
  *
  * @param decls declarations that assign an id to an expression
  * @param in the expression where the ids declared in [[decls]] will be visible
  */
case class Let (decls : Map[Id, Expr], in : Expr) extends Expr

/** Allows the logic to branch out based on the value of a reference expression.
  *
  * This is similar to pattern matching in functional languages and SQL's case statements. It can also
  * be used as a replacement for traditional if/else constructs by setting the reference expression
  * to constant `true`.
  *
  * @param ref the reference expression to match the cases against
  * @param cases pair of expressions, the first component is the pattern to match, the second is the expression to return
  */
case class Case (ref : Expr, cases : Seq[(Expr, Expr)]) extends Expr

