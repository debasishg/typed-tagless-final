package infin

import cats._
import cats.data._
import cats.implicits._

object Intro1 {

  /**
   * section 2.1 : Initial encoding
   * Deep / Initial embedding
   */ 
  
  // The data type of expressions
  trait Exp
  case class Lit(i: Int) extends Exp
  case class Neg(e: Exp) extends Exp
  case class Add(l: Exp, r: Exp) extends Exp
  

  // A sample expression: our first running example
  val ti1 = Add(Lit(8), (Neg(Add(Lit(1), Lit(2)))))
  
  // The evaluator
  // It proceeds by case analysis on the expression
  def eval: Exp => Int = { e: Exp =>
    e match {
      case Lit(i) => i
      case Neg(exp) => - eval(exp)
      case Add(e1, e2) => eval(e1) + eval(e2)
    }
  }

  val ti1_eval = eval(ti1)
  // 5

  // A different way to write the evaluator
  // using the constructor functions
  type Repr = Int

  def lit: Int => Repr = identity
  def neg: Repr => Repr = r => -r
  def add: Repr => Repr => Repr = e1 => e2 => e1 + e2

  /**
   * evaluation for final encoding
   * Oleg says "The computation is compositional: the meaning of, for example, addition is computed
   * from the meaning, the value of, the summands. We see the first intimation
   * of the denotational semantics, with further to come."
   */ 
  
  // Our sample expression in the final form:
  // I just downcased ti1
  val tf1 = add(lit(8))(neg(add(lit(1))(lit(2))))
  // 5


  // We will call the shallow metacircular embedding the final way
  //
  // But the datatype way (initial way) lets us "evaluate" ti1 in a different way:
  // to pretty-print it
  def view: Exp => String = { e: Exp =>
    e match {
      case Lit(i) => i.show
      case Neg(exp) => s"(-${view(exp)})"
      case Add(e1, e2) => s"(${view(e1)} + ${view(e2)})"
    }
  }

  val ti1_view = view(ti1)
  // "(8 + (-(1 + 2)))"
  
  /**
   * How can we evaluate tf1 differently? It seems that the evaluator is hard-wired into tf1...
   * Indeed, the constructor functions lit, neg, and add all have
   * return type that is set to Int. The evaluator `view' returns a string.
   * So, at the very least we must find a way to parameterize the constructor
   * functions by the result type.
   */ 
}
