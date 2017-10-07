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

    def showsPerson: String = Person("Rob", 32).shows

    /**
     * Enum
     *
     * 'a' to 'c'    - NumericRange(a, b, c)
     * 'a' |-> 'c'   - List(a, b, c)
     */
  }

  object FunctorMonadApplicative{
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

    /** it's exactly the same as composition:  f andThen g */
    def andThen_toLowerCase = ((s: String) => s.toLowerCase) andThen (_ + "...")

    /**
     * Functors additionally have some operators for overriding values:
     */
    def functorForList_overridesElements1 = List(1,2,3) as "x" // List (x, x, x)
    def functorForList_overridesElements2 = List(1,2,3) >| "x" // List (x, x, x)
  }
}

