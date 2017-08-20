package infin

import cats._
import cats.data._
import cats.implicits._

object FlatI {
  import Intro1._
  import infin.{PushNegI => PNeg}
  import PNeg._

  /**
   * Flatten the additions using the associativity
   * * (A + B) + R => A + (B + R)
   * Draw the trees for the former and the latter
   * The goal is to convert the addition tree to the right-skewed form
   * The transformation is assumed to be performed after the negation is
   * pushed down
   * 
   * Previously, expressions were constructed according to this grammar:
   * * General grammar of expressions
   * *     e ::= int | neg e | add e e
   * *
   * * Restricted grammar now:
   * *     e ::= factor | add factor e
   * *     factor ::= int | neg int
   * Now, only integer literals can be negated, and only once.
   * 
   * It is an expression transformer
   */ 

   def flata: Exp => Exp = { e =>
     e match {
       case e@Lit(_) => e
       case e@Neg(_) => e
       case Add(Add(e1, e2), e3) => flata (Add(e1, Add(e2, e3)))
       case Add(e1, e2) => Add(e1, (flata(e2)))
     }
   }

   /**
    * Why is the above code hard to comprehend ?
    *
    * Oleg says:
    *
    * "The code literally implements the algorithm of the repeated reassociation-to-theright,
    * assuming that only literals are negated. Unlike the pushing of negations, we
    * repeatedly process the transformed expression in the last-but-one clause, which
    * is patently not structurally inductive. The termination, and hence, correctness, is
    * even harder to see. To show the termination, we have to introduce lexicographic
    * ordering on the left- and the overall depths of a term. The nested pattern-match
    * again betrays the context-sensitivity of the transformation."
    */ 

    /**
     * Why is this terminating?
     * The last two clauses express the lexicographic ordering
     * on left-depth, total depth.
     * Is this code correct?
     */ 

    def norm: Exp => Exp = flata compose push_neg

    val ti3 = Add(ti1, (Neg(Neg(ti1))))
    val ti3_view = view(ti3)
    // res0: String = ((8 + (-(1 + 2))) + (-(-(8 + (-(1 + 2))))))
  
    val ti3_eval = eval(ti3)
    // res1: Int = 10
    
    // The normalized expression can be evaluated with any interpreter
    val ti3_norm = norm(ti3)

    val ti3_norm_view = view(ti3_norm)
    // res0: String = (8 + ((-1) + ((-2) + (8 + (-(1 + 2))))))
  
    // The result of the standard evaluation (the `meaning') is preserved
    val ti3_norm_eval = eval(ti3_norm)
    // res1: Int = 10
}

