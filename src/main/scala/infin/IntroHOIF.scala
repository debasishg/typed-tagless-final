package infin

/**
 * Tagless initial and final evaluators for simply-typed lambda-calculus
 * based on the code accompanying the paper by Jacques Carette, Oleg Kiselyov, and Chung-chieh Shan
 * The language is simply typed lambda-calculus with booleans,
 * and de Bruijn encoding of variables.
 */ 

object IntroHOIF {

  class Tuple0 extends Product {
    override def productElement(n: Int): Any = throw new NoSuchElementException()
    override def productArity: Int = 0
    override def canEqual(that: Any): Boolean = false
 
    override def toString: String = "()"
  }

  /**
   * The initial encoding
   * 
   * From our experience with the tagfull evaluator, we learned
   * that we need to parameterize the expression type by the
   * type of the result and by the environment, so to distinguish
   * the open terms.
   * We need GADTs. In fact, the present example was the motivation
   * for GADTs.
   */ 

  object Initial {
    // The initial encoding
    sealed trait Var[Env, T]
    case class VZ[Env, T]() extends Var[(T, Env), T]
    case class VS[Env, T, X](v: Var[Env, T]) extends Var[(X, Env), T]
  
    sealed trait Exp[Env, T]
    case class B[Env](b: Boolean) extends Exp[Env, Boolean]
    case class V[Env, T](v: Var[Env, T]) extends Exp[Env, T]
    case class L[Env, X, Y](t: Exp[(X, Env), Y]) extends Exp[Env, X => Y]  {type P = X}
    case class A[Env, X, Y](e1: Exp[Env, X => Y], e2: Exp[Env, X]) extends Exp[Env, Y]

    /**
     * Exp represents only well-typed object terms
     * Axioms of minimal logic
     * We can read the axioms and inference rules of minimal logic just
     * from the above definition
     */ 

    /**
     * This is like lookp in IntroHOT.hs, only the environment is a
     * heterogeneous list (a nested tuple) rather than an ordinary list
     */ 
    def lookp[Env, T](v: Var[Env, T], e: Env): T = (v, e) match {
      case (VZ(), (x, _)) => x 
      case (VS(w), (_, env)) => lookp(w, env)
      case _ => ???
    }
  
    /**
     * This is exactly the evaluator we wanted to write in IntroHOT.hs
     * With GADTs, we can.
     * No longer universal type, no longer type tagging, no longer
     * partial pattern matching (in A and in lookp)
     */ 
    def eval[Env, T](env: Env, exp: Exp[Env, T]): T = exp match {
      case V(v) => lookp(v, env)
      case B(b) => b
      case ll@L(e) => { x: ll.P => eval((x, env), e) }
      case A(e1, e2) => eval(env, e1)(eval(env, e2))
    }
  
    def ti1[Env]: Exp[Env, Boolean] =
      A(L(V(VZ[Env, Boolean])),B(true))
  
    val ti1_eval = eval[Tuple0, Boolean](new Tuple0, ti1)
  
    /*
    def ti2[Env]: Exp[Env, Boolean] = 
      A(
        A(
          L(
            L(
              V(
                VS(
                  VZ
                )
              )
            )
          ), 
          B(true)
        ), 
        B(false)
      )
    
    val ti2_eval = eval[Tuple0, Boolean](new Tuple0, ti2)
    */
  
    // def ti2o[Env]: Exp[Env, Boolean] = A(L(V(VS[Env, Boolean, Boolean](VZ[Env, Boolean]))), B(true))
  }
  
  object Final {
    def vz[A, B](a: A, b: B): A = a
    def vs[A, B, T](vp: B => T)(envl: A, envr: B): T = vp(envr)
    def b[Env](bv: Boolean)(env: Env): Boolean = bv
    def l[A, Env, T](e: (A, Env) => T)(env: Env): A => T = x => e(x, env)
    def a[A, Env, T](e1: Env => A => T, e2: Env => A)(env: Env) = e1(env)(e2(env))
  
    def tf1[Env]: Env => Boolean = a(l[Boolean, Env, Boolean](vz) _, b(true))
    def tf3[A, B](env: (A, B)): A =
      a(l(vs((vz(_:A,_:B)).tupled)), b(true))(env)
  
    val tf1_eval = tf1(())
    // res0: Boolean = true
    
    /*
    def tf2[A, B](env: (A, B)): A = 
      a(
        a(
          l(
            l(vs((vz(_:A, _:B)).tupled))
          ), 
          b(true)
        )(env), 
        b(false)
      )(env)
  
    val tf2_eval = tf2 ()
    */
  }
}
