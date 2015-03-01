package ozmi.lambda_core

sealed abstract class TypeDesc
case class NamedType (name : Id) extends TypeDesc
/** owl:oneOf */
case class EnumType (values : Value*) extends TypeDesc
/** OWL Property Restrictions */
case class RestrictedType (baseType : TypeDesc, constraints : Constraint*) extends TypeDesc
/** No OWL mapping */
case class FilterType (baseType : TypeDesc, predicate : Lambda) extends TypeDesc
case class MapType (baseType : TypeDesc, mapping : Map[Id, Lambda]) extends TypeDesc
case class JoinType (leftType : TypeDesc, rightType : TypeDesc) extends TypeDesc

/** OWL Property Restrictions */
sealed abstract class Constraint
/** owl:allValuesFrom */
case class TypeConstraint (function : Id, signature : TypeDesc*) extends Constraint
/** owl:hasValue */
case class ValueConstraint (function : Id, values : Value*) extends Constraint
/** owl:minCardinality, owl:maxCardinality, owl:cardinality */
case class CardinalityConstraint (function : Id, signature : Cardinality*) extends Constraint

case class Cardinality (min : Int, max : Option[Int])

/** OWL Class Axioms 
  * rdfs:subClassOf
  * owl:equivalentClass
  * owl:disjointWith
  */

/** Calculated properties should not be tied to a specific type. They should be bound by signature or at least it should be possible to do that.
  */


abstract class SubtypeOf (superType : TypeDesc) {
    
    def apply (typeDesc : TypeDesc) : Expr =
        TypeLiteral (typeDesc)
    
    def unapply (expr : Expr) : Option[TypeDesc] = Some(expr) collect {
        case TypeLiteral (subType) if isSubtypeOf (subType, superType) => subType
    }
    
    def isSubtypeOf (subType : TypeDesc, superType : TypeDesc) : Boolean = ???
    
}