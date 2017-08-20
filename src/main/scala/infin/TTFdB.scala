package infin

import cats._
import cats.data._
import cats.implicits._

/**
 * * Typed tagless-final interpreters for simply-typed lambda-calculus
 * * de Bruijn indices
 * based on the code accompanying the paper by Jacques Carette, Oleg Kiselyov, and Chung-chieh Shan
 */ 

object TTFdB {

  trait Symantics[Repr[_, _]] {
    def int[H]: Int => Repr[H, Int]
    def add[H]: Repr[H, Int] => Repr[H, Int] => Repr[H, Int]

    def z[H, A]: Repr[(A, H), A]
    def s[H, A, B]: Repr[H, A] => Repr[(B, H), A]
    def lam[H, A, B]: Repr[(A, H), B] => Repr[H, A => B]           // implication introduction
    def app[H, A, B]: Repr[H, A => B] => Repr[H, A] => Repr[H, B]  // implication elimination
  }

  /**
   * Like GADT-based, but parametric
   * Like ExpSYM, but repr is of kind * -> * -> *
   * repr is parameterized by the environment (h) and the type
   * of the expression
   *
   * * The types read like the rules of minimal logic
   * For example, z is the axiom stating that assuming A we can get A
   * lam is the inference rule: if assuming A we derive B,
   * then we can derive the implication A->B
   *
   * Notes from the original Haskell code:
   *
   * * Type system of simply-typed lambda-calculus in Haskell98!
   * The signature of the class can be read off as the typing
   * rules for simply-typed lambda-calculus. But the code
   * is Haskell98! So, contrary to the common belief, we do not
   * need dependent types to express the type system of simply
   * typed lambda-calculus. Compare with Term.agda
   *
   * We are writing Scala though and we need to help the compiler infer types.
   *
   * * ``a way to write a typed fold function over a typed term.''
   * as one reviewer of our paper put it
   */ 

  // sample terms
  def td1[H, Repr[_, _]](implicit is: Symantics[Repr]): Repr[H, Int] = is.add(is.int(1))(is.int(2))

  def td2[H, Repr[_, _]](implicit is: Symantics[Repr]): Repr[H, Int => Int] = {
    import is._
    println("in td2")
    lam(add(z[H, Int])(z[H, Int]))
  }

  def td2o[H, Repr[_, _]](implicit is: Symantics[Repr]): Repr[(Int, H), (Int => Int)] = {
    import is._
    lam(add(z[(Int, H), Int])(s[(Int, H), Int, Int](z[H, Int])))
  }

  def td3[H, Repr[_, _]](implicit is: Symantics[Repr]): Repr[H, (Int => Int) => Int] = {
    import is._
    lam(
      add
        (app[(Int => Int, H), Int, Int]
          (z[H, Int => Int])
          (int[(Int => Int, H)](1))
        )
        (int[(Int => Int, H)](2))
    )
  }

  /**
   * Ill-typed terms are not expressible
   * * td2a = app (int 1) (int 2)
   * Couldn't match expected type `a -> b' against inferred type `Int'
   * Expected type: repr h (a -> b)
   * Inferred type: repr h Int
   * In the first argument of `app', namely `(int 1)'
   */ 

  /**
   * Embedding all and only typed terms of the object language
   * in the typed metalanguage
   * Typed object terms are represented as typed Haskell terms
   */ 

  /** 
   * Typed and tagless evaluator
   *  object term ==> metalanguage value
   */  

  class Tuple0 extends Product {
    override def productElement(n: Int): Any = throw new NoSuchElementException()
    override def productArity: Int = 0
    override def canEqual(that: Any): Boolean = false
 
    override def toString: String = "()"
  }

  object Evaluator {
    case class R[H, A](val unR: H => A) 
  
    implicit def RSymantics = new Symantics[R] {
      def int[H]: Int => R[H, Int] = (x: Int) => R((h: H) => x)
      def add[H]: R[H, Int] => R[H, Int] => R[H, Int] = e1 => e2 => R((h: H) => e1.unR(h) + e2.unR(h)) 
  
      def z[H, A]: R[(A, H), A] = R((h: (A, H)) => h._1)
      def s[H, A, B]: R[H, A] => R[(B, H), A] = (v => R((h: (B, H)) => v.unR(h._2)))
      def lam[H, A, B]: R[(A, H), B] => R[H, A => B] = (e => R((h: H) => x => e.unR((x, h))))
      def app[H, A, B]: R[H, A => B] => R[H, A] => R[H, B] = (e1 => e2 => R((h: H) => (e1.unR(h))(e2.unR(h))))
    }
  
    def eval[T](r: R[Tuple0, T]) = r.unR(new Tuple0)
  
    val td1_eval = eval(td1)
    // res0: Int = 3
  
    val td2_eval = eval(td2)
    // res0: Int => Int = infin.TTFdB$$anon$1$$Lambda$5064/1463224324@1ea646e3
  
    val td2_eval1 = eval(td2)(21)
    // res1: Int = 42
  
    // val td2o_eval = eval(td2o)
  
    val td3_eval = eval(td3)
    // res0: (Int => Int) => Int = infin.TTFdB$$anon$1$$Lambda$5324/1788210569@7eaa1c41
  
    val td3_eval1 = eval(td3)(_ * 2)
    // res1: Int = 4
  }

  // Another interpreter

  case class S[H, A](val unS: Int => String) 

  implicit def SSymantics = new Symantics[S] {
    def int[H]: Int => S[H, Int] = (x: Int) => S(_ => x.show)

    def add[H]: S[H, Int] => S[H, Int] => S[H, Int] = e1 => e2 => S{(h: Int) => 
      s"( ${e1.unS(h)} + ${e2.unS(h)} )"
    }

    def z[H, A]: S[(A, H), A] = S((h: Int) => s"x${(h - 1).show}")

    def s[H, A, B]: S[H, A] => S[(B, H), A] = (v => S((h: Int) => v.unS(h - 1)))

    def lam[H, A, B]: S[(A, H), B] => S[H, A => B] = e => 
      S{(h: Int) => { 
        val x = "x" + h.show
        s"(\\\\$x -> ${e.unS(h + 1)})"
      }}

    def app[H, A, B]: S[H, A => B] => S[H, A] => S[H, B] = e1 => e2 => 
      S((h: Int) => s"( ${e1.unS(h)} ${e2.unS(h)} )")
  }

  def view[T](s: S[Int, T]): String = s.unS(0)

  val td1_view = view(td1)
  // res2: String = ( 1 + 2 )

  val td2_view = view(td2)
  // res0: String = (\\x0 -> ( x0 + x0 ))

  val td3_view = view(td3)
  // res1: String = (\\x0 -> ( ( x0 1 ) + 2 ))

  /**
   * We now finally see our terms in a useful form
   * Clearly, de Bruijn notation is not the most perspicuous
   * We now turn to HOAS
   */ 
}
