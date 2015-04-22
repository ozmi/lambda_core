package ozmi.lambda_core.sql

import org.scalatest.FunSuite

/**
  * Created by attila on 4/20/2015.
  */
class SqlPrinterTest extends FunSuite {

     test ("precedence 1") {
         assert (
             SqlPrinter.print (BinaryOp ("+", IntegerLit (1), BinaryOp ("*", IntegerLit (2), IntegerLit (3))))
                 === "1 + 2 * 3")
     }

     test ("precedence 2") {
         assert (
             SqlPrinter.print (BinaryOp ("*", BinaryOp ("+", IntegerLit (1), IntegerLit (2)), IntegerLit (3)))
                 === "(1 + 2) * 3")
     }

     test ("precedence 4") {
         assert (
             SqlPrinter.print (BinaryOp ("+", IntegerLit (1), BinaryOp ("*", UnaryOp ("-", IntegerLit (2)), IntegerLit (3))))
                 === "1 + - 2 * 3")
     }

 }
