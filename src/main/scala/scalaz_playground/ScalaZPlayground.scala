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

    /**
     * if there is implicit Semigroup[A], ScalaZ implements an imlpicit Monoid[Option[A]]
     * so I can add it up even with zero element (Monoid) :
     *
     * Some(1) |+| Some(1) |+| None |+| Some(1) |+| None  // ==> 3
     */

    implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
      def append(maybeA1: Option[A], maybeA2: => Option[A]) = (maybeA1, maybeA2) match {
        case (Some(a1), Some(a2)) => Some(a1 |+| a2)
        case (Some(a1), None) => maybeA1
        case (None, Some(a2)) => maybeA2
        case (None, None) => None
      }
      def zero: Option[A] = None
    }

  }

  object ApplicativeBuilderWithValidation{
    /** Just a reminder what is Applicative:
     *
     *   trait Applicative2[A] {
     *       def apply[B](f: Applicative2[A => B]): Applicative2[B]
     *       def unit[T](a: T): Applicative2[T]
     *   }
     */

    // shitty cause it takes only 2 arguments
    def applicativeOldStyle =  ^(3.some, 5.some) (_ + _) // Some(8)

    // ScalaZ applicative builder - can chain many Validations
    def applicativeValidation = (3.some |@| 5.some |@| 0.some) (_ + _ + _) // Some(8)

    /**
     * Validation is similar to \/ cause :
     * \/         - has implementations: \/- and -\/,
     * Validation - has implementations: Success and Failure
     *
     * \/         is monad       - first fail stops calculations in chain of flatMap (for-comprehension)
     * Validation is applicative - any fail will contribute to common error result.
     */

    def validationSuccess[Err, Suc](t: Suc): Validation[Err, Suc] = Success[Suc](t) // or t.success[Err]
    def validationFailure[Err, Suc](err: Err): Validation[Err, Suc] = Failure[Err](err) // or err.failure[Suc]

    def validation = (
        "1".success[String] |@|
        "2".failure[String] |@|
        "3".failure[String]) (_ + _ + _)  // Failure("23")

    /**
     * The problem with this validation is that error is glued together - we'd prefer a list
     *
     * Nel - NotEmptyList
     */
    def validationNel = (
        "1".successNel[String] |@|
        "2".failureNel[String] |@|
        "3".failureNel[String]) (_ + _ + _)  // Failure(NonEmptyList( "2", "3"))

    /**
     * Real example:
     */
    case class TestApp(
      config: Map[String, String] // or ConfigFactory.load(s"conf/${hostName.value}")
    ) {

      private def optional(property: String): ValidationNel[String, Option[String]] =
        (config.get(property): Option[String]).success

      private def required(property: String): ValidationNel[String, String] =
        (config.get(property): Option[String]).toSuccess(s"property $property not set").toValidationNel

      private val maybeProp1: ValidationNel[String, Option[String]] = optional("1")
      private val maybeProp2: ValidationNel[String, Option[String]] = optional("2")
      private val prop3: ValidationNel[String, String] = required("3")

      val partialConfig: Validation[NonEmptyList[String], PartialConfig] =
        (maybeProp1 |@| maybeProp2 |@| prop3) (PartialConfig.apply)
      /* PartialConfig.apply takes Strings (or Option[String]) and returns Config file - or list of failures */

      //val appConfig = (partialConfig |@| jms |@| db |@| others )(AppConfig.apply)
      // /* AppConfig.apply  takes different configs and return App Config*/

    }
    case class PartialConfig(maybeProp1: Option[String], maybeProp2: Option[String], prop3: String)

  }

  object UsefulMonadicFunctions {

    /**
     * .join  works like flattening
     */
    def joinedOptionOfOption: Option[Int] = (Some(Some(9)): Option[Option[Int]]).join // Some(9)
    def joinedListOfList: List[Int] = List(List(1, 2, 3), List(4, 5, 6)).join // List(1,2,3,4,5,6)

    /**
     * .filterM works like all combinations:
     */
    def sth: List[List[Int]] = List(1, 2, 3) filterM { x => List(true, false) }
    // List( List(1), List(2), List(3), List(1,2), List(2,3), List(1,3), List(1,2,3), Nil )

  }
}

