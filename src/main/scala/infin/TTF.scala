package infin

/**
 * Typed tagless-final interpreters for PCF
 * Higher-order abstract syntax
 * based on the code accompanying the paper by Jacques Carette, Oleg Kiselyov, and Chung-chieh Shan
 */

import cats._
import cats.data._
import cats.implicits._

object TTF {

  /**
   * The language is simply-typed lambda-calculus with fixpoint and
   * constants. It is essentially PCF.
   * The language is just expressive enough for the power function.
   * We define the language by parts, to emphasize modularity.
   * The core plus the fixpoint is the language described in the paper
   *    Hongwei Xi, Chiyan Chen, Gang Chen
   *    Guarded Recursive Datatype Constructors, POPL2003
   * which is used to justify GADTs.
   */ 

  // core language
  trait Symantics[Repr[_]] {
    def int: Int => Repr[Int]
    def add: Repr[Int] => Repr[Int] => Repr[Int]

    def lam[A, B]: (Repr[A] => Repr[B]) => Repr[A => B]
    def app[A, B]: Repr[A => B] => Repr[A] => Repr[B]
  }

  /**
   * * Like ExpSYM, but repr is of kind * -> *
   * repr is parameterized by the type of the expression
   * * The types read like the minimal logic in 
   * * Gentzen-style natural deduction
   * * Type system of simply-typed lambda-calculus in Haskell98!
   */

  // Sample terms and their inferred types
  // They do look better now.
  
  def th1[Repr[_]](implicit ri: Symantics[Repr]): Repr[Int] = {
    import ri._
    add(int(1))(int(2))
  }
  
  def th2[Repr[_]](implicit ri: Symantics[Repr]): Repr[Int => Int] = {
    import ri._
    lam(x => add(x)(x))
  }

  def th3[Repr[_]](implicit ri: Symantics[Repr]) = {
    import ri._
    lam((x: Repr[Int => Int]) => add(app(x)(int(1)))(int(2)))
  }

  // Typed and tagless interpreter
  object Evaluator {
    case class R[A](val unR: A) extends AnyVal 
  
    implicit val RSymantics = new Symantics[R] {
      def int: Int => R[Int] = R(_)
      def add: R[Int] => R[Int] => R[Int] = e1 => e2 => R(e1.unR + e2.unR)
      def lam[A, B]: (R[A] => R[B]) => R[A => B] = f => R(x => f(R(x)).unR)
      def app[A, B]: R[A => B] => R[A] => R[B] = rab => ra => R((rab.unR)(ra.unR))
    }
  
    /**
     * Notes for Haskell implementation:
     *
     * * R is not a tag! It is a newtype
     * The expression with unR _looks_ like tag introduction and
     * elimination. But the function unR is *total*. There is no 
     * run-time error is possible at all -- and this fact is fully 
     * apparent to the compiler.
     *
     * Furthermore, at run-time, (R x) is indistinguishable from x
     * * R is a meta-circular interpreter
     * This is easier to see now. So, object-level addition is
     * _truly_ the metalanguage addition. Needless to say, that is
     * efficient.
     * * R never gets stuck: no pattern-matching of any kind
     * * R is total
     *
     * Scala implementation: We are using value class - hence here also R is 
     * not a tag and the compiler understands this. We don't have any runtime overhead.
     */ 

    def eval[T](e: R[T]): T = e.unR
    
    val th1_eval = eval(th1)
    // res0: Int = 3
  
    val th2_eval: Int => Int = eval(th2)
    val th2_eval1 = eval(th2)(21)
    // res2: Int = 42
  
    val th3_eval: (Int => Int) => Int = eval(th3)
    val th3_eval1 = eval(th3)(_ * 2)
    // res2: Int = 4
  }

  object ShowInterpreter {
    case class S[A](val unS: Int => String) extends AnyVal 
  
    implicit def SSymantics = new Symantics[S] {
      def int: Int => S[Int] = x => S(h => Function.const(x.show)(h))
  
      def add: S[Int] => S[Int] => S[Int] = e1 => e2 => S(h => s"( ${e1.unS(h)} + ${e2.unS(h)} )")
  
      def lam[A, B]: (S[A] => S[B]) => S[A => B] = f => S { h =>
        val x = s"x${h.show}"
        s"(\\\\$x -> ${f(S(Function.const(x)(_: Int))).unS(h + 1)})"
      }
  
      def app[A, B]: S[A => B] => S[A] => S[B] = sab => sa => S(h => s"(${sab.unS(h)} ${sa.unS(h)} )")

      // The major difference from TTFdb is the interpretation
      // of lam
    }
  
    def view[T](s: S[T]): String = s.unS(0)
  
    val th1_view = view(th1)
    // res0: String = ( 1 + 2 )
  
    val th2_view = view(th2)
    // res1: String = (\\x0 -> ( x0 + x0 ))
  
    val th3_view = view(th3)
    // res2: String = (\\x0 -> ( (x0 1 ) + 2 ))

    /**
     * * The crucial role of repr being a type constructor
     * rather than just a type. It lets some information about
     * object-term representation through (the type) while
     * keeping the representation itself hidden.
     */ 

  }

  // Extensions of the language
  //
  // Multiplication
  trait MulSYM[Repr[_]] {
    def mul: Repr[Int] => Repr[Int] => Repr[Int]
  }

  // Booleans
  trait BoolSYM[Repr[_]] {
    def bool: Boolean => Repr[Boolean]
    def leq: Repr[Int] => Repr[Int] => Repr[Boolean]
    
    // changes from Haskell implementation : eager evaluation by default in Scala
    def if_[A]: Repr[Boolean] => (() => Repr[A]) => (() => Repr[A]) => Repr[A]
  }

  // Fixpoint
  trait FixSYM[Repr[_]] {
    def fix[A]: ((=> Repr[A]) => Repr[A]) => Repr[A]
  }
  
  // * Extensions are independent of each other

  /** 
   * Compose all to form the bigger language. Note this is the way to compose
   * tagless final abstractions in Scala. Much simpler than initial ones or free monads
   */ 
  trait LambdaExpr[Repr[_]] extends Symantics[Repr] 
    with MulSYM[Repr]
    with BoolSYM[Repr]
    with FixSYM[Repr]

  /**
   * Now we can define our own combinators using the language. Note we are being
   * completely generic here - no implementation has been made so far.
   */ 
  def tpow[Repr[_]](implicit li: LambdaExpr[Repr]): Repr[Int => Int => Int] = {

    import li._

    lam(x => 
      fix[Int => Int](self => 
        lam {n => 
          if_(leq(n)(int(0)))(() => int(1))(() => mul(x)(app(self)(add(n)(int(-1)))))
        }
      )
    )
  }

  def tpow7[Repr[_]](implicit li: LambdaExpr[Repr]) = { 

    import li._
    lam((x: Repr[Int]) => app(app(tpow)(x))(int(7)))
  }

  def tpow72[Repr[_]](implicit li: LambdaExpr[Repr]) = { 

    import li._
    app(tpow7)(int(2))
  }

  // Now we have one implementation for evaluation. We can just import
  // the earlier implementations and add new ones.
  
  object EvaluatorExt {
    import Evaluator.{R, eval}
  
    implicit val RLambdaExpr = new LambdaExpr[R] {
      import Evaluator.RSymantics
  
      def int: Int => R[Int] = RSymantics.int
      def add: R[Int] => R[Int] => R[Int] = RSymantics.add
      def lam[A, B]: (R[A] => R[B]) => R[A => B] = RSymantics.lam
      def app[A, B]: R[A => B] => R[A] => R[B] = RSymantics.app
  
      def mul: R[Int] => R[Int] => R[Int] = e1 => e2 => R(e1.unR * e2.unR)
  
      def bool: Boolean => R[Boolean] = R(_)
  
      def leq: R[Int] => R[Int] => R[Boolean] = e1 => e2 => R(e1.unR <= e2.unR)
  
      def if_[A]: R[Boolean] => (() => R[A]) => (() => R[A]) => R[A] = be => et => ee => 
        R(if (be.unR) et().unR else ee().unR)
  
      // changes from Haskell implementation : eager evaluation by default in Scala
      def fix[A]: ((=> R[A]) => R[A]) => R[A] = f => {
        def fx(f: (=> R[A]) => R[A]): R[A] = {
          lazy val ra: R[A] = f(ra)
          ra
        }
        R(fx(f).unR)
      }
    }
  
    val tpow_eval = eval(tpow)
    // res0: Int => (Int => Int) = infin.TTF$Evaluator$$anon$1$$Lambda$4235/204503734@5c41de28
  
    val tpow72_eval = eval(tpow72)
    // res1: Int = 128
  }

  // Extending the S interpreter
  object ShowInterpreterExt {
    import ShowInterpreter.{S, view}
  
    implicit val SLambdaExpr = new LambdaExpr[S] {
      import ShowInterpreter.SSymantics
  
      def int: Int => S[Int] = SSymantics.int
      def add: S[Int] => S[Int] => S[Int] = SSymantics.add
      def lam[A, B]: (S[A] => S[B]) => S[A => B] = SSymantics.lam
      def app[A, B]: S[A => B] => S[A] => S[B] = SSymantics.app
  
      def mul: S[Int] => S[Int] => S[Int] = e1 => e2 => S(h => s"(${e1.unS(h)} * ${e2.unS(h)})")
  
      def bool: Boolean => S[Boolean] = x => S(Function.const(x.show)(_))
  
      def leq: S[Int] => S[Int] => S[Boolean] = e1 => e2 => S(h => s"(${e1.unS(h)} <= ${e2.unS(h)})")
  
      def if_[A]: S[Boolean] => (() => S[A]) => (() => S[A]) => S[A] = be => et => ee => 
        S( h => s"if ${(be.unS(h))} then ${et().unS(h)} else ${ee().unS(h)}")
  
      def fix[A]: ((=> S[A]) => S[A]) => S[A] = e => S{ h => 
        val self = s"self${h.show}"
        s"(fix $self.${e(S(Function.const(self)(_: Int))).unS(h + 1)})"
      }
    }

    val tpow_view = view(tpow)
    // res0: String = (\\x0 -> (fix self1.(\\x2 -> if (x2 <= 0) then 1 else (x0 * (self1 ( x2 + -1 ) )))))
  }

  /**
   * Oleg writes:
   *
   * The typed tagless final encoding may be called translucent: it hides concrete representations 
   * yet exposes enough of the type information to type check the encod- ing of an object term without 
   * knowing its concrete representation. The checked term is then well-typed in any interpreter, 
   * for any instantiation of repr. The higher-order polymorphism, quantifying over type variables 
   * like repr of higher kind, is essential. 
   */ 
}
