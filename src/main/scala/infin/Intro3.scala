package infin

import cats._
import cats.data._
import cats.implicits._

object Intro3 {
  import infin.{Intro1 => I}
  import infin.{Intro2 => F}

  import I.{Exp, Lit, Neg, Add}
  import F._

  // Remind what is initial Exp, and what is a final ExpSYM. Discuss `initial' vs `final' from the categorical view.
  // A bit informally, I would be calling ExpSYM `final' (waving hands over the existence of the final co-algebra)

  // We can put expressions represented as a data type in the same list

  val til1: List[Exp] = List(Lit(1), Add(Lit(1), Lit(3)), I.ti1)

  // And we can evaluate them uniformly
  val til1_eval = til1 map I.eval
  // List(1, 4, 5)
  
  val til1_view = til1 map I.view
  // List("1","(1 + 3)","(8 + (-(1 + 2)))")
  
  // Can we do the same for expressions represented in the final form?
  def tfl1[Repr](implicit sym: ExpSYM[Repr]) = {
    import sym._
    List(lit(1), add(lit(1))(lit(3)), tf1[Repr])
  }

  val tfl1_eval = tfl1[Int] map eval
  // res0: List[Int] = List(1, 4, 5)

  val tfl1_view = tfl1[String] map view
  // res1: List[String] = List(1, (1 + 3), (8 + (-(1 + 2))))
}

