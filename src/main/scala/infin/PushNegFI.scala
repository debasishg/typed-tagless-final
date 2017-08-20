package infin

import cats._
import cats.data._
import cats.implicits._

/**
 * Demonstrating `non-compositional', context-sensitive processing
 * The final style, via the intermediate data type.
 * This is cheating. But it does show the relationship between
 * initial and final. It also points out that everything we can
 * do with the data type, we can do in the final approach too -- albeit
 * with cheating in this case
 */ 
object PushNegFI {

  import infin.Intro2._
  import infin.Intro1.{Exp, Lit, Neg, Add}
  import infin.{PushNegI => I}
  import I._

  // We write the interpreter for final terms that produce initial, data type terms
  implicit val expExpSYM = new ExpSYM[Exp] {

    def lit: Int => Exp = Lit
    def neg: Exp => Exp = Neg
    def add: Exp => Exp => Exp = (e1 => e2 => Add(e1, e2))
  }

  def initialize: Exp => Exp = identity

  // We write an evaluator for data type that produces final representation
  // The analogy with fold must be patent.
  def finalize[Repr: ExpSYM]: Exp => Repr = { exp =>

    val i = implicitly[ExpSYM[Repr]]
    import i._

    exp match {
      case Lit(n) => lit(n)
      case Neg(e) => neg(finalize[Repr](i)(e))
      case Add(l, r) => add(finalize[Repr](i)(l))(finalize[Repr](i)(r))
    }
  }

  // Now, push_neg in the final style is a mere composition
  def push_neg[Repr: ExpSYM] = finalize[Repr] compose I.push_neg compose initialize

  // To remind, here is our sample term
  val tf1_view = view(tf1[String])
  // res0: String = (8 + (-(1 + 2)))

  def tf1_norm[Repr](implicit i: ExpSYM[Repr]) = push_neg[Repr](i)(tf1[Exp])

  // The new expression can be evaluated with any interpreter
  val tf1_norm_view = view(tf1_norm[String])
  // res1: String = (8 + ((-1) + (-2)))

  val tf1_norm_eval = eval(tf1_norm[Int])
  // res1: Int = 5

  // Add an extra negation
  def tf1n_norm[Repr](implicit i: ExpSYM[Repr]) = push_neg[Repr](i)(expExpSYM.neg(tf1[Exp]))

  // see the result
  val tf1n_norm_view = view(tf1n_norm[String])
  // res0: String = ((-8) + (1 + 2))

  val tf1n_norm_eval = eval(tf1n_norm[Int])
  // res1: Int = -5

  // Negate the already negated term
  def tf1nn_norm[Repr](implicit i: ExpSYM[Repr]) = push_neg[Repr](i)(expExpSYM.neg(tf1n_norm[Exp]))

  val tf1nn_norm_view = view(tf1nn_norm[String])
  // res0: String = (8 + ((-1) + (-2)))

  /**
   * The initial and the closed-to-language-extensions final approaches can also
   * be related most straightforwardly (see the file PushNegFI.hs), by transforming
   * a finally-encoded term to the corresponding data-type-encoded term, and vice
   * versa. The relation thus is a bijection, witnessed by two total interpreters: interpreting
   * a finally-encoded term as a data type
   */ 
}
