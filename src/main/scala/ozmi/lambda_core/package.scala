package ozmi

package object lambda_core {

    /** Id is the type of unique identifiers in the expression tree.
      *
      * It is currently just a type alias for String but it might be changed to a
      * different data structure in the future. The choice of an unstructured data
      * type was a conscious decision though to prevent tools from relying on
      * information possibly encoded in the id which should only be used for
      * identification based on exact match and should not carry any semantic meaning.
      */
    type Id = String

    implicit def stringToLookup (id : String) : Lookup = Lookup (id)
    
}