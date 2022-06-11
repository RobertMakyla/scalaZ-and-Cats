package playground

import scalaz._
import Scalaz._

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

  object Functors{
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
     * Validation Applicative - Real example:
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

  object MonadTransformers {

    /** Useful with stacked Monads: M[N[A]] (where M and N are Monads)
     *
     * for 2 things :
     *
     * DE-NESTING TO REDUCE BOILERPLATE:
     *    When you have 2 nested for-comprehensions, you might want to use Monad Transformers for clarity
     *
     * TRANSFORMING:
     *    (eg List[Option] to have functionaliy of Option[List] )
     */

    // De-NESTING TO REDUCE BOILERPLATE

    def ls1: List[Option[Int]] = List(1.some,2.some)  // .some is nicer than Some() cause it returns type Option, not Some
    def ls2: List[Option[Int]] = List(100.some)

    def stackedMonadsUglyNested: List[Option[Int]] = for {
      o1: Option[Int] <- ls1
      o2: Option[Int] <- ls2
    } yield for {
      i1: Int <- o1
      i2: Int <- o2
    } yield i1 + i2 // List(101.some, 102.some)

    /**
     * OptionT()  - takes argument M[Option[A]]  (eg List[Option] )
     * EitherT()  - takes argument M[Either[A]]
     */
    def stackedMonadsDeNested: List[Option[Int]] = {
      val result: OptionT[List, Int] = for {
        i1: Int <- OptionT(ls1)
        i2: Int <- OptionT(ls2)
      } yield i1 + i2
      result.run // run zwraca typ początkowy - ten który wrzucaliśmy do OptionT( ... )
    }

    // TRANSFORMING

    /**
     * List[Option] --> OptionT[List, Int]
     *
     * and this has all functionality of Option:
     *
     * OptionT[List, Int].isDefined  ---> List[Boolean]
     */

    val listOfOptions: List[Option[Int]] = List(1.some, 2.some)
    val optionT_of_list: OptionT[List, Int] = OptionT(listOfOptions)
    def isDefinedOptionT: List[Boolean] = optionT_of_list.isDefined // List(true, true)

    /**
     * Option[Either[String, Int]] --> EitherT[Option, String, Int]
     *
     * and this has all functionality of Either:
     */
    val optionOfEither: Option[\/[String, Int]] = 1.right[String].some
    val eitherT_of_option: EitherT[Option, String, Int] = EitherT(optionOfEither)

    def isRightEitherT: Option[Boolean] = eitherT_of_option.isRight // true
    def isLeftEitherT: Option[Boolean] = eitherT_of_option.isLeft // false
  }

  object Lenses {

    /**
     * Lenses - for updating immutable complex, nested structures
     */

    case class TheName(value: String)
    case class Person(name: TheName)
    case class Contract(person: Person)

    val johnsContract = Contract(Person(TheName("John")))

    val newContract =
      johnsContract.copy(
        person = johnsContract.person.copy(
          name = johnsContract.person.name.copy(
            value = "NewSomeone"
          )))

    /**
     * imperative way for mutable fields:
     *
     *  a.b.c.d.e += 1
     *
     * functional way if we want to keep immutability (too much boilerplate)
     *
     * a.copy(
     *   b = a.b.copy(
     *     c = a.b.c.copy(
     *       d = a.b.c.d.copy(
     *         e = a.b.c.d.e + 1
     * ))))
     *
     */

    //defining lenses

    val contactsPerson = Lens.lensu[Contract, Person](
      (a, value) => a.copy(person = value),
      _.person
    )
    val personsName = Lens.lensu[Person, TheName](
      (a, value) => a.copy(name = value),
      _.name
    )
    val namesValue = Lens.lensu[TheName, String](
      (a, value) => a.copy(value = value),
      _.value
    )

    // using lenses

    val fromContactToNameValue = contactsPerson >=> personsName >=> namesValue // shortcut all the way Contract-to-String
    fromContactToNameValue.get(johnsContract) // "John"
    fromContactToNameValue.set(johnsContract, "James") // Whole Contact with James

    /**
     * The End word:
     *
     * Using ScalaZ lens is complex - there's a lot to define before we can use it ..
     * however it can still save our ass ..
     *
     * much simpler are quicklenses from softwaremill ..
     */
  }

  /**
   * Other stuff:
   *
   * Arrow - scalaZ abstraction for Function1[A,B], PartialFunction1[A,B], Kleisli [F[_], A]
   *
   */

  object Memorization {

    /**
     * Memo - wrapping slow function (eg recursive function).
     * It gives result much faster by caching some results in RAM - it's called MEMORIZATION.
     *
     * sealed trait Memo{
     * def apply(z: K => V): K => V
     * }
     *
     */
    val slowFibonacci: Int => Int = {
      case 0 => 0
      case 1 => 1
      case n => slowFibonacci(n - 2) + slowFibonacci(n - 1)
    }


    val fastFibonacci: Int => Int = Memo.mutableHashMapMemo {
      case 0 => 0
      case 1 => 1
      case n => fastFibonacci(n - 2) + fastFibonacci(n - 1)
    }
  }

  object IO_Monad {
    import scalaz._, Scalaz._, effect._, IO._

    def sideEffectGiving1: IO[Int] = IO{
      println(s"I will now return 1") // side effect
      1
    }

    def sideEffectMultiplyBy10(n: Int): IO[Int] = IO {
      println(s"I will now multiply $n by 10") // side effect
      n * 10
    }

    val sideEffect100: IO[Int] = for { // sideEffect100.unsafePerformIO()  -->  100
      one <- sideEffectGiving1
      ten <- sideEffectMultiplyBy10(one)
      hundred <- sideEffectMultiplyBy10(ten)
    } yield hundred

    /**
     * Pros:
     * - guarantee of sequencial execution (of side effects/mutations)
     * - MODULARITY - as it's monad we can compose many IOMonads
     */

    // You can use |+| instead of for-comprehension, if IO's don't escalate values
    val sideEffect300 = sideEffect100 |+| sideEffect100 |+| sideEffect100

    /**
     * Cons - no guarantee that we will ever return from IO Monad execution
     */

    /**
     * Conclusion
     *
     * Instead of using IO effects everywhere in code, we can separate IO code from the one with no effects.
     * => like this, we can be more sure about the code with no effects
     * => like this, we can still compose IO code in FP way - into sequences of operations
     */
  }
}

