package infin

import cats._
import cats.data._
import cats.implicits._

// Demonstrating non-compositional, context-sensitive processing
// The initial style

object PushNegI {

  // * Compositionality
  //    The meaning of a complex expression is determined by its
  //    structure and the meanings of its constituents.
  //    http://plato.stanford.edu/entries/compositionality/
  // The epitome:
  //        eval (Add e1 e2) = eval e1 + eval e2
  //
  //    - Evaluators and other interpreters are compositional
  //    - Denotational semantics must be compositional
  //    - Compositionality is modularity
  //    - Compositionality is context-insensitivity
  //    - Bottom-up reconstruction of meaning
  //    - Compositional processing is `fold'
  //
  //  The final approach embodies `fold'
  //
  // Optimization and other such program transformations give many
  // examples of apparently non-compositional processing

  import infin.Intro1._
  
  // Interpreters that we saw up till now are compositional. But not all operations are compositional.
  // There are however many operations – for example, program transformations and optimizations – that do not seem 
  // compositional because the handling of a sub-expression does depend on where it appears in a larger expression (i.e.,
  // depends on the context).
  //
  // section 2.4: Pushing negation down : the initial view (making seemingly non compositional operations compose)
  // Oleg says:
  //
  // "The type of push_neg emphasizes that we are transforming one expression to another; the result is an embedded 
  // expression in its own right and can be processed with any existing interpreter. The transformed expression 
  // should be equivalent to the source with respect to a set of laws"
  def push_neg: Exp => Exp = { e: Exp =>
    e match {
      case Lit(_) => e
      case (Neg(Lit(_))) => e
      case Neg(Neg(n)) => n
      case Neg(Add(e1, e2)) => Add(push_neg(Neg(e1)), push_neg(Neg(e2)))
      case Add(e1, e2) => Add(push_neg(e1), push_neg(e2))
    }
  }

  // to remind, here is our sample term
  val ti1_view = view(ti1)
  // res0: String = (8 + (-(1 + 2)))

  val ti1_norm = push_neg(ti1)

  // The new expression can be evaluated with any interpreter
  val ti1_norm_view = view(ti1_norm)
  // res0: String = (8 + ((-1) + (-2)))

  // The result of the standard evaluation (the `meaning') is preserved
  val ti1_norm_eval = eval(ti1_norm)
  // res1: Int = 5

  // Add an extra negation
  val ti1n_norm = push_neg(Neg(ti1))

  // See the result and its value
  val ti1n_norm_view = view(ti1n_norm)
  // res0: String = ((-8) + (1 + 2))
  val ti1n_norm_eval = eval(ti1n_norm)
  // res1: Int = -5

  // Negate the already negated term
  val ti1nn_norm = push_neg(Neg(ti1n_norm))

  val ti1nn_norm_view = view(ti1nn_norm)
  // res0: String = (8 + ((-1) + (-2)))
}

