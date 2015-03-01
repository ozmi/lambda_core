package ozmi.lambda_core
package lib

import org.scalacheck.Prop.forAll
import org.scalacheck.{Gen, Properties}

object CollSpec extends Properties("Coll") {
    
    import ozmi.lambda_core.lib.Coll._
    import ozmi.lambda_core.lib.Num._
    import ozmi.lambda_core.lib.Ord._
    
    lazy val saneBigDecimal = 
        for {
            unscaledVal   <- Gen.choose(-1000, 1000)
            scale         <- Gen.choose(0, 100)
        } yield 
            BigDecimal(unscaledVal, scale)
        
    lazy val saneBigDecimalSeq =
        Gen.containerOf[Seq, BigDecimal](saneBigDecimal)

    property("Filter") = forAll (saneBigDecimalSeq) { (coll: Seq[BigDecimal]) =>
        val predicate    = (i: BigDecimal) => i < 100
        val predicateEx  = Lambda (Seq ("i"), LessThan (Lookup ("i"), Literal (BigDecimal (100))))
        Library.eval (Filter (Literal (coll), predicateEx)) == Literal (coll filter predicate)
    }
    
    property("Map") = forAll (saneBigDecimalSeq) { (coll: Seq[BigDecimal]) =>
        val mapping    = (i: BigDecimal) => i * 2
        val mappingEx  = Lambda (Seq ("i"), Multiply (Lookup ("i"), Literal (BigDecimal (2))))
        Library.eval (Map (Literal (coll), mappingEx)) == Literal (coll map mapping)
    }
    
    property("Reduce") = forAll (saneBigDecimalSeq) { (coll: Seq[BigDecimal]) =>
        val reduction    = (a: BigDecimal, b: BigDecimal) => a + b
        val reductionEx  = Lambda (Seq ("a", "b"), Add (Lookup ("a"), Lookup ("b")))
        val expected =
            if (coll.isEmpty) Literal (null)
            else Literal (coll reduce reduction)
        Library.eval (Reduce (Literal (coll), reductionEx)) == expected
    }
    
}