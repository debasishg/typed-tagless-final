package infin

import cats._
import cats.data._
import cats.implicits._

// Lack of extensibility in the data type view

object ExtI {
  import infin.{Intro1 => Old}

  // Attempt to add a new expression form: multiplication
  // We would like to reuse the old code, in Intro1.hs (show the code)
  // We don't want to extend the data type declaration in Intro1.hs
  // as that would require adjusting and recompiling all the code
  // that uses Intro1.hs (in particular, all the interpreters)
  // 
  trait Exp
  case class EOld(oldExp: Old.Exp) extends Exp
  case class Mul(e1: Exp, e2: Exp) extends Exp   // add a new variant

  // An extended sample expression
  val tim2 = Mul(EOld(Old.Lit(7)), EOld(Old.ti1))

  // The following does not typecheck!
  // [error] type mismatch;
  // [error]  found   : infin.ExtI.Mul
  // [error]  required: infin.Intro1.Exp
  // [error]           Mul(
  // [error]              ^
  // [error] one error found

  //val tim1 = 
  //  EOld(
  //    Old.Add(
  //      Old.Lit(7),
  //      Old.Neg(
  //        Mul(
  //          EOld(Old.Lit(1)),
  //          EOld(Old.Lit(2))
  //        )
  //      )
  //    )
  //  )
  //
  // So, we are stuck. Data type variants are NOT extensible.
  //  
}
