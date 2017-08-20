package infin

import cats._
import cats.data._
import cats.implicits._

object PushNegFExt {

  import Intro2._    // Exp in final form
  import PushNegF._  // push_neg interpreter
  import ExtF._      // mul extension

  implicit def ctxMulSYM[Repr: MulSYM] = new MulSYM[Ctx => Repr] {

    val r = implicitly[MulSYM[Repr]]

    // Unlike addition, multiplication is not a homomorphism with respect to negation!
    // neg (a * b) /= (neg a) * (neg b)
    def mul: (Ctx => Repr) => (Ctx => Repr) => Ctx => Repr = { e1 => e2 => c =>
      c match {
        case Pos => r.mul(e1(Pos))(e2(Pos))
        case Neg => r.mul(e1(Pos))(e2(Neg))
      }
    }
  }

  val tfm1_view = view(tfm1[String])
  // res0: String = (7 + (-(1 * 2)))

  val tfm1_eval = eval(tfm1[Int])
  // 5

  def tfm1_norm[Repr: ExpSYM : MulSYM] = push_neg[Repr](tfm1[Ctx => Repr])

  // The new expression can be evaluated with any interpreter
  val tfm1_norm_view = view(tfm1_norm[String])
  // res0: String = (7 + (1 * (-2)))

  // The result of the standard evaluation (the `meaning') is preserved
  val tfm1_norm_eval = eval(tfm1_norm[Int])
  // res2: Int = 5

  // Add an extra negation
  def tfm1n_norm[Repr: ExpSYM : MulSYM] = push_neg[Repr]((implicitly[ExpSYM[Ctx => Repr]]).neg(tfm1[Ctx => Repr]))

  // see the result
  val tfm1n_norm_view = view(tfm1n_norm[String])
  // res0: String = ((-7) + (1 * 2))

  val tfm1n_norm_eval = eval(tfm1n_norm[Int])
  // res1: Int = -5

  // Negate the already negated term
  def tfm1nn_norm[Repr: ExpSYM : MulSYM] = {
    val n = implicitly[ExpSYM[Ctx => Repr]]
    import n._
    push_neg[Repr](neg(tfm1n_norm[Ctx => Repr]))
  }

  val tfm1nn_norm_view = view(tfm1nn_norm[String])
  // res2: String = (7 + (1 * (-2)))

  val tfm1nn_norm_eval = eval(tfm1nn_norm[Int])
  // res3: Int = 5

  /**
   * The same for tmf2
   * We can even use a previously defined unextended expression (tf1)
   * as a part of the extended expression.
   * We can indeed mix-and-match
   */ 

  def tfm2_norm[Repr: ExpSYM : MulSYM] = push_neg[Repr](tfm2[Ctx => Repr])

  // The new expression can be evaluated with any interpreter
  val tfm2_norm_view = view(tfm2_norm[String])
  // res0: String = (7 * (8 + ((-1) + (-2))))

  // The result of the standard evaluation (the `meaning') is preserved
  val tfm2_norm_eval = eval(tfm2_norm[Int])
  // res2: Int = 35

  // Add an extra negation
  def tfm2n_norm[Repr: ExpSYM : MulSYM] = push_neg[Repr]((implicitly[ExpSYM[Ctx => Repr]]).neg(tfm2[Ctx => Repr]))

  // see the result
  val tfm2n_norm_view = view(tfm2n_norm[String])
  // res2: String = (7 * ((-8) + (1 + 2)))

  val tfm2n_norm_eval = eval(tfm2n_norm[Int])
  // res1: Int = -35

  // Negate the already negated term
  def tfm2nn_norm[Repr: ExpSYM : MulSYM] = {
    val n = implicitly[ExpSYM[Ctx => Repr]]
    import n._
    push_neg[Repr](neg(tfm2n_norm[Ctx => Repr]))
  }

  val tfm2nn_norm_view = view(tfm2nn_norm[String])
  // res4: String = (7 * (8 + ((-1) + (-2))))

  val tfm2nn_norm_eval = eval(tfm1nn_norm[Int])
  // res3: Int = 35
}
