package infin

import cats._
import cats.data._
import cats.implicits._

object PushNegF {

  import infin.Intro2._

  /**
   * section 2.4: Pushing negation down: the final view
   *
   * The operation of pushing negation down is indeed non-compositional, because the processing of a negated expression 
   * depends on its context. To be precise, it depends on whether the negated expression appears as part of a negated
   * expression. We make that context-dependence explicit:
   */ 
  sealed trait Ctx
  case object Pos extends Ctx
  case object Neg extends Ctx

  implicit def ctxExpSYM[Repr: ExpSYM] = new ExpSYM[Ctx => Repr] {

    val r = implicitly[ExpSYM[Repr]]
    
    def lit: Int => Ctx => Repr = { n => c =>
      c match {
        case Pos => r.lit(n)
        case Neg => r.neg(r.lit(n))
      }
    }
    def neg: (Ctx => Repr) => Ctx => Repr = { e => c =>
      c match {
        case Pos => e(Neg)
        case Neg => e(Pos)
      }
    }
    def add: (Ctx => Repr) => (Ctx => Repr) => Ctx => Repr = { e1 => e2 => c =>
      r.add(e1(c))(e2(c)) // homomorphism
    }
  }

  /**
   * The interpreter for pushing the negation down
   * The result is a tagless-final term, which can be interpreted in many ways; for example, pretty-printing 
   * view (push neg tf1) gives ”(8 + ((-1) + (-2)))”.
   */ 
  def push_neg[Repr]: (Ctx => Repr) => Repr = _(Pos)

  val tf1_view = view(tf1[String])
  // res1: String = (8 + (-(1 + 2)))

  def tf1_norm[Repr: ExpSYM] = push_neg[Repr](tf1[Ctx => Repr])

  // The new expression can be evaluated with any interpreter

  val tf1_norm_view = view(tf1_norm[String])
  // res0: String = (8 + ((-1) + (-2)))

  /**
   * The negation pushing transformation is more apparent in the final encoding. e.g. It's just a homomorphism
   * with respect to addition. Have a look at tf1_view and tf1_norm_view - the homomorphism is evident.
   */ 

  // The result of the standard evaluation (the `meaning') is preserved
  val tf1_norm_eval = eval(tf1_norm[Int])
  // res1: Int = 5

  // Add an extra negation
  def tf1n_norm[Repr: ExpSYM] = push_neg[Repr]((implicitly[ExpSYM[Ctx => Repr]]).neg(tf1[Ctx => Repr]))

  val tf1n_norm_view = view(tf1n_norm[String])
  // res0: String = ((-8) + (1 + 2))

  // The result of the standard evaluation (the `meaning') is preserved
  val tf1n_norm_eval = eval(tf1n_norm[Int])
  // res1: Int = -5

  // Negate the already negated term
  def tf1nn_norm[Repr: ExpSYM] = {
    val n = implicitly[ExpSYM[Ctx => Repr]]
    import n._
    push_neg[Repr](neg(tf1n_norm[Ctx => Repr]))
  }

  val tf1nn_norm_view = view(tf1nn_norm[String])
  // res0: String = (8 + ((-1) + (-2)))
}

