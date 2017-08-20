package infin

import cats._
import cats.data._
import cats.implicits._

object FlatF {
  
  import Intro2._
  import infin.{PushNegF => Neg}
  import Neg.{Ctx => NCtx, _}

  sealed trait Ctx[+E]
  case class LCA[E](e: E) extends Ctx[E]
  case object NonLCA extends Ctx[Nothing]

  implicit def lcaCtxExpSYM[Repr: ExpSYM] = new ExpSYM[Ctx[Repr] => Repr] {

    val r = implicitly[ExpSYM[Repr]]
    
    def lit: Int => Ctx[Repr] => Repr = { n => c =>
      c match {
        case NonLCA => r.lit(n)
        case LCA(e) => r.add(r.lit(n))(e)
      }
    }
    def neg: (Ctx[Repr] => Repr) => Ctx[Repr] => Repr = { e => c =>
      c match {
        case NonLCA => r.neg(e(NonLCA))
        case LCA(e3) => r.add(r.neg(e(NonLCA)))(e3)
      }
    }
    def add: (Ctx[Repr] => Repr) => (Ctx[Repr] => Repr) => Ctx[Repr] => Repr = { e1 => e2 => c =>
      e1(LCA(e2(c)))
    }
  }

  def flata[Repr]: (Ctx[Repr] => Repr) => Repr = _(NonLCA)

  // def norm[Repr](t: NCtx => Repr) = flata[Repr] compose push_neg[Repr](t)

  def tf3[Repr: ExpSYM]: Repr = {
    val r = implicitly[ExpSYM[Repr]]
    import r._
    add(tf1[Repr])(neg(neg(tf1[Repr])))
  }

  val tf3_view = view(tf3[String])
  // res0: String = ((8 + (-(1 + 2))) + (-(-(8 + (-(1 + 2))))))
  
  val tf3_eval = eval(tf3[Int])
  // res0: Int = 10

  def tf4[Repr: ExpSYM] = {
    val r = implicitly[ExpSYM[Repr]]
    import r._
    val t = add(add(lit(1))(lit(2)))(lit(3))
    add(t)(neg(t))
  }

  val tf4_view = view(tf4[String])
  // res0: String = (((1 + 2) + 3) + (-((1 + 2) + 3)))
  
  val tf4_eval = eval(tf4[Int])
  // res0: Int = 0
}
