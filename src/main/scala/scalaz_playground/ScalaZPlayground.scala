package scalaz_playground

import scalaz.Scalaz._
import scalaz._

object ScalaZPlayground {

  object ComparingInstances {

    /**
     * Equal - fails compilation if types don't match
     *
     *    ===    equals
     *    /==    not equals
     *    =/=    not equals (the same as /==)
     */
    def equalTypeUnsafe[A, B](a: A, b: B): Boolean = a == b
    def equalTypeSafeForInts(a: Int, b: Int): Boolean = a === b
    def notEqualTypeSafeForInts(a: Int, b: Int): Boolean = a /== b

    /** defining my own Equal[Person] */
    case class Person(name: String, age: Int)

    object Person {
      implicit def personEqual = new Equal[Person] {
        override def equal(a1: Person, a2: Person): Boolean = a1.name == a2.name && a1.age == a2.age
      }
    }
    def equalTypeSafeForPerson(a: Person, b: Person): Boolean = a === b
    def notEqualTypeSafeForPerson(a: Person, b: Person): Boolean = a =/= b

    // A Context Bound 'T : Equal' requires that there is implicit value of Equal[T]
    def equalTypeSafe[T: Equal](a: T, b: T) = a === b

    /**
     * Order - fails compilation if types don't match (eg Int and Double)
     *
     * gt
     * lt
     * max
     */
    def isBiggerTypeSafe(a: Int, b: Int) = a gt b

    /**
     * Show
     */
    implicit val personShow = new Show[Person] {
      override def shows(a: Person) = s"name: ${a.name}, age: ${a.age}"
    }

    def showsPersonRob32: String = Person("Rob", 32).shows

    // A Context Bound 'T : Show' requires that there is implicit value of Show[T]
    def showsAnything[T: Show](t: T) = t.shows

    /**
     * Enum
     *
     * 'a' to 'c'    - NumericRange(a, b, c)
     * 'a' |-> 'c'   - List(a, b, c)
     */
  }

  object FunctorApplicativeMonad{
    /** Just a reminder what is functor:
     *
     *  trait Functor[A] {
     *     def map[B](f: A => B): Functor[B]
     *     def unit(a: A): Functor[A]
     *  }
     */

    /**
     * Functors - ScalaZ has some interesting implicit functors
     *
     * Thanks to it, I can use  map() (or ∘() )  on things I wouldn't expect to have map()
     *
     * The '∘' is alias for map
     */

    def functorForTuple = (1, 2, 3) ∘ (_ * 100) // however it applies only on last element (1, 2, 300)

    def functorForFunction1_toUpperCase = ((s: String) => s.toUpperCase) map (_ + "...")
    def functorForFunction1_toLowerCase = ((s: String) => s.toLowerCase) ∘ (_ + "...")

    /** functor for function it's exactly the same as composition:  f andThen g */
    def andThen_toLowerCase = ((s: String) => s.toLowerCase) andThen (_ + "...")

    /**
     * Functors additionally have some operators for overriding values:
     */
    def functorForList_overridesElements1 = List(1,2,3) as "x" // List (x, x, x)
    def functorForList_overridesElements2 = List(1,2,3) >| "x" // List (x, x, x)

    /** Just a reminder what is Applicative:
     *
     *   trait Applicative2[A] {
     *       def apply[B](f: Applicative2[A => B]): Applicative2[B]
     *       def unit[T](a: T): Applicative2[T]
     *   }
     */

    def applicativeOldStyle =  ^(3.some, 5.some) (_ + _) // Some(8)

    // ScalaZ applicative builder
    def applicativeValidation = (3.some |@| 5.some) (_ + _) // Some(8)
  }

  object SemigroupsMonoidsGroups {
    /**
     * In "Category Theory" we have following categories :
     *
     * Magma     - type T and some binary operation (not associative)
     * Semigroup - type T + associative operation
     * Monoid    - type T + associative operation + identity element (zero element)
     * Group     - type T + associative operation + identity element (zero element)
     *             + invertibility (for each element 'a: T' there is another 'b:T' for which op(a,b) == zero element
     */

    /**
     * In ScalaZ :
     * Semigroup's associative operator is: |+| or ⊹ or mappend
     * Monoids's identity element is:       .zero
     */

    // A Context Bound 'T : Monoid' requires that there is implicit value of Monoid[T]
    def semigroupOperator[T: Semigroup](a: T, b: T) = a |+| b
    def monoidIdentity[T: Monoid] = Monoid[T].zero // eg: monoidIdentity[List[Int]]  gives  List.empty[Int]
    def addingIdentityToMonoid[T: Monoid](t: T): T = t |+| Monoid[T].zero
  }
}

