package infin

import cats._
import cats.data._
import cats.implicits._

/**
 * A Tagfull evaluator for simply-typed lambda-calculus
 * This example is borrowed from the introduction section
 * of the paper
 * * Emir Pa{\v s}ali{\'c} and Walid Taha and Tim Sheard
 * * Tagless Staged Interpreters for Typed Languages, ICFP2002.
 * It is discussed in detail in the tagless-final paper by
 * Jacques Carette, Oleg Kiselyov, and Chung-chieh Shan
 */ 

/**
 * The illustration of problems of embedding a typed DSL into a typed metalanguage 
 * Either the Universal type (and hence spurious partiality, type tags and inefficiency), 
 * or fancy type systems seem inevitable. The problem stems from algebraic data types' 
 * being too broad: they express not only well-typed DSL terms but also ill-typed ones.
 */ 
object IntroHOT {

  /**
   * The language is simply typed lambda-calculus with booleans,
   * and de Bruijn encoding of variables.
   */ 

  sealed trait Var
  case object VZ extends Var
  case class VS(v: Var) extends Var

  sealed trait Exp
  case class V(v: Var) extends Exp
  case class B(b: Boolean) extends Exp
  case class L(e: Exp) extends Exp
  case class A(e1: Exp, e2: Exp) extends Exp

  // Unsuccessful attempt to write the evaluator
  //
  /**
   * def eval0(env: Env, exp: Exp): U = exp match {
   *   case V(v) => lookup(v, env)
   *   case B(b) => b
   *   case L(e) => { x => eval0((x :: env), e) }
   *   case A(e1, e2) => (eval0(env, e1), eval0(env, e2))
   * }
   *
   * Expressed in a typed language, eval0 is ill-typed, which we can tell even without 
   * seeing the implementation of lookp. The second clause returns a boolean b whereas 
   * the next one returns a Haskell function, which cannot be of the type Bool. All branches 
   * of a pattern-match on ordinary algebraic data types must yield values of the same type. 
   * We have little choice but introduce the union type for booleans and functions, the universal type
   */ 

  // We have to introduce the universal type. This has the tag to differentiate
  // between the 2 types - whether we have the Boolean type or the function type
  sealed trait U
  case class UB(b: Boolean) extends U
  case class UA(uf: U => U) extends U

  implicit val uShow = Show.show[U] { u: U => 
    u match {
      case UB(b) => s"UB ${b.show}"
      case UA(_) => s"UA <fun>"
    }
  }

  type Env = List[U]
  def lookup(v: Var, env: Env): U = v match {
    case VZ => env.head
    case VS(v) => lookup(v, env.tail)
  }

  // Now we can write the evaluator
  def eval(env: Env, exp: Exp): U = exp match {
    case V(v) => lookup(v, env)
    case B(b) => UB(b)
    case L(e) => UA { x: U => eval((x :: env), e) }
    case A(e1, e2) => (eval(env, e1): @unchecked) match {
      case UA(f) => f(eval(env, e2))
    }
  }

  final val empty_env = List.empty[U]

  // A sample term
  val ti1 = A (L(V(VZ)), B(true))
  val ti1_eval = eval(empty_env, ti1)
  // res0: infin.IntroHOT.U = UB(true)

  // partial pattern match in the A clause
  // So, we can get stuck if we try to apply a Boolean
  val ti2a = A(B(true), B(false))
  val ti2a_eval = eval(empty_env, ti2a)
  // scala.MatchError: UB(true) (of class infin.IntroHOT$UB)
  //  ... 39 elided

  // partial pattern match in lookp
  // So, we can get stuck if we try to evaluate an open term
  val ti2o = A(L(V(VS(VZ))), B(true))
  val ti2o_eval = eval(empty_env, ti2o)
  // scala.MatchError: UB(true) (of class infin.IntroHOT$UB)
  //  ... 39 elided

  /**
   * tagging and untagging of the UA tag
   * tagging overhead, part of the interpretive overhead

   * But well-typed programs don't go wrong!
   * The terms ti2a and ti2o are ill-typed in the object language
   * If an object term is well-typed, it shall be
   * interpreted without any pattern-match failure.
   * So, how to communicate to the system that runs the interpreter 
   * that the object term is well-typed, so the pattern-match
   * code could be compiled efficiently, without any error-handling.
   * 
   * * Problem: algebraic data types Exp and Var express more terms that
   * * all well-typed terms in simply-typed lambda-calculus
   * * Run-time checks in eval become necessary
   * Because we can express ill-typed terms, we have all the run-time
   * checks in the form of type tags UA/UB and empty-list checks
   * in lookp
   */ 

   /**
    * <em>Oleg explains why our tagful interpreters failed:</em>
    *
    * Thus the presence of the type tags like UB and UA and run-time tag checking are symptoms of the 
    * problem of embedding typed object languages. Informally, our embedding is not ‘tight’: the algebraic 
    * data type Exp contains more values than there are well-typed terms in the simply typed lambda-calculus 
    * with booleans. The embedding failed to represent the well-formedness constraints imposed by the object 
    * language’s type system.
    */ 
}


