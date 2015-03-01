package ozmi.lambda_core
package lib

import org.scalacheck.Prop._
import org.scalacheck.{Gen, Properties}

/**
 * Created by attila on 3/1/2015.
 */
object PersonRowSpec extends Properties("PersonRow") {

    case class Person (age : BigInt, friends : Seq[Person]) extends ProductRow (Seq ("age", "friends"))

    def personGen (depth : Int) : Gen[Person] =
        if (depth == 0) {
            for (age <- Gen.choose (0, 100)) yield Person (age, Seq ())
        } else {
            for {
                age <- Gen.choose (0, 100)
                friends <- Gen.containerOf[Seq, Person] (personGen (depth - 1))
            } yield
                Person (age, friends)
        }

    property("age") = forAll (personGen (1)) { (p: Person) =>
        Library.eval (Apply (Lookup ("age"), Literal (p))) == Literal (p.age)
    }

    property("friends") = forAll (personGen (1)) { (p: Person) =>
        Library.eval (Apply (Lookup ("friends"), Literal (p))) == Literal (p.friends)
    }

}