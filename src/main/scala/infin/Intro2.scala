package infin

import cats._
import cats.data._
import cats.implicits._

object Intro2 {
  /**
   * section 2.1: Oleg continues ..
   *
   * "In the final encoding, the evaluator is hardwired into the representation tf1, making it impossible 
   * to interpret the object term as something other than integer. We want the final embedding to permit
   * multiple interpretations too; we must therefore find a way to parameterize the ‘constructor functions’ 
   * such as lit and neg by the result type."
   * The right tool for this parameterization is the typeclass approach
   */ 

  // Parameterizing the final interpreter over the type Repr
  trait ExpSYM[Repr] {
  
    /**
     * The constructor functions lit, neg and add have essentially the same signatures as before; the repr 
     * type however is now variable. The declaration of ExpSYM should remind us even more of the denotational 
     * semantics, over the semantic domain repr. As befits denotational semantics, the meaning of an expression, 
     * whatever repr happens to be, is computed from the meanings of the components
     */ 
    def lit: Int => Repr
    def neg: Repr => Repr
    def add: Repr => Repr => Repr
  }

  /**
   * Compared to the signatures of lit, neg and add in Intro1.scala, Repr is now a type parameter
   * * cf. Denotational Semantics
   * The declaration of ExpSYM should remind one even more of
   * denotational semantics: think of 'Repr' as the semantic domain
   * The declaration of ExpSYM patently expresses that semantics is
   * _compositional_
   *
   * Our sample expression in final form
   * Note: The difference with Haskell is the following:
   * In Haskell the running example has the form
   *
   * tf1 = add (lit 8) (neg (add (lit 1) (lit 2)))
   *  
   * but the inferred type (provided we disable Haskell’s monomorphism restriction) is different. It's no 
   * longer Repr (that is, Int). Rather, it is ExpSYM repr ⇒ repr, polymorphic over the semantic domain. 
   * An object term is represented not by its abstract syntax but by its meaning, denotation in a semantic domain.
  
   * With Scala we don't have the type inference like Haskell, but with standard typeclass encoding we provide
   * the ExpSYM[Repr] as an implicit argument. Hence effectively it's also ExpSYM[Repr] => Repr
   */ 
  def tf1[Repr: ExpSYM]: Repr = {
    val e = implicitly[ExpSYM[Repr]]
    import e._
    add(lit(8))(neg(add(lit(1))(lit(2))))
  }

  /**
   * We represent object terms not by its abstract syntax (initial) but by its denotation in a semantic algebra.
   * That's why we call the approach `final'
   *
   * To interpret finally-encoded expressions, we write an instance for ExpSYM, specifying the semantic domain. 
   * For example, we may interpret expressions as integers
   */ 
  implicit val intExpSYM = new ExpSYM[Int] {
    val lit = identity
    val neg = (r: Int) => -r
    val add = e1 => e2 => e1 + e2
  }

  /**
   * Interpreters in the final approach: Oleg says:
   *
   * "Our embedded language interpreters have been compositional; they define the language’s denotational semantics, 
   * which is required to be compositional. The compositional interpretation of a term is epitomized in a fold. 
   * Our interpreters are all folds. In the final approach, the fold is ‘wired in’ in the definition of the interpreters. 
   * Compositionality, or context-insensitivity, lets us build the meaning of a larger expression bottom-up, from leaves 
   * to the root. Again, in the final approach, that mode of computation is hard-wired in."
   *
   * interpreter 1 : evaluator of the final encoded term
   * Oleg says: 
   *
   * "The function eval has a strange type for an evaluator, and an even stranger definition – 
   * the identity function. It is more proper to call eval a selector of an interpretation as an integer. 
   * A finally-encoded expression has an indefinite number of interpretations; eval selects one of them."
   */ 
  def eval: Int => Int = identity

  // Of course, the more precise name for eval would be ``an interpretation that returns an integer''
  // There may be many interpretations that return an Int. We should use `newtype' wrappers to distinguish them.
  val tf1_eval = eval(tf1[Int])
  // 5

  // SYM stands for Symantics: the class defines the syntax, and instances define the semantics
  // Now we can write a different evaluator
  implicit val stringExpSYM = new ExpSYM[String] {
    val lit = _.show
    val neg = (r: String) => s"(-$r)"
    val add = e1 => e2 => s"($e1 + $e2)"
  }
  //
  // The pretty-printing interpreter is again the identity; indeed it does not do anything. Only its type matters, 
  // which selects from the multitude of, one may imagine, already computed interpretations.
  def view: String => String = identity

  val tf1_view = view(tf1[String])
  // "(8 + (-(1 + 2)))"
  //
  // One may love this approach: view and eval don't do anything (they are the identity), but give the right
  // result anyway.
  // 
}


