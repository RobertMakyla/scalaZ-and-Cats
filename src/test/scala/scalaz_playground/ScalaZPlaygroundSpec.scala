package scalaz_playground

import org.scalatest.{FreeSpec, MustMatchers}

import scalaz._
import Scalaz._

class ScalaZPlaygroundSpec extends FreeSpec with MustMatchers {

  "Comparing, Showing instances" - {
    import ScalaZPlayground.ComparingInstances._

    "Equals - verifies type at compile-time, while scala's == always compiles..." in {
      equalTypeUnsafe(1, "") mustBe false // very risky thing that it actually compiles

      equalTypeSafeForInts(1, 1) mustBe true
      equalTypeSafe(1, 1) mustBe true
      notEqualTypeSafeForInts(1, 1) mustBe false

      equalTypeSafeForPerson(Person("Rob", 32), Person("Mon", 31)) mustBe false
      equalTypeSafe(Person("Rob", 32), Person("Mon", 31)) mustBe false
      notEqualTypeSafeForPerson(Person("Rob", 32), Person("Mon", 31)) mustBe true
    }

    "Ordering" in {
      isBiggerTypeSafe(100, 200) mustBe false
    }

    "Show: Cord / Shows: String " in {
      Person("Rob", 33).shows mustBe "name: Rob, age: 33"

      showsPersonRob32                 mustBe "name: Rob, age: 32"
      showsAnything(Person("Rob", 32)) mustBe "name: Rob, age: 32"
    }
  }

  "Functor, Applicative, Monad" - {
    import ScalaZPlayground.Functors._

    "functor for tuples" in {
      functorForTuple mustBe ((1, 2, 300))
    }
    "functor for Function1[String]" in {
      functorForFunction1_toUpperCase("robert") mustBe "ROBERT..."
      functorForFunction1_toLowerCase("ROBERT") mustBe "robert..."
      andThen_toLowerCase("ROBERT") mustBe "robert..."
    }
    "functor for List - overriding elements" in {
      functorForList_overridesElements1 mustBe List("x", "x", "x")
      functorForList_overridesElements2 mustBe List("x", "x", "x")
    }

    "Semigroups Monoids Groups" in {
      import ScalaZPlayground.SemigroupsMonoidsGroups._

      semigroupOperator(List(1,2,3), List(4,5,6)) mustBe List(1,2,3,4,5,6)

      monoidIdentity[String] mustBe ""
      monoidIdentity[Int] mustBe 0
      monoidIdentity[List[Char]] mustBe List.empty[Char]

      addingIdentityToMonoid("anything") mustBe "anything"
      addingIdentityToMonoid(List('a, 'b)) mustBe List('a, 'b)
    }

    "Monoid of Option" in {
      import ScalaZPlayground.SemigroupsMonoidsGroups.optionMonoid

      1.some |+| 1.some |+| None |+| 1.some |+| None mustBe Some(3)
    }

    "ApplicativeBuilder with Validation" - {
      import ScalaZPlayground.ApplicativeBuilderWithValidation._

      "applicative" in {
        applicativeOldStyle mustBe Some(8)
        applicativeValidation mustBe Some(8)
      }

      "Validation" in {
        validation mustBe Failure("23")
        validationNel mustBe Failure(NonEmptyList( "2", "3"))
      }

      "Real app example with Validation" in {
        TestApp(Map("1" -> "one", "2" -> "two", "3" -> "three")).partialConfig mustBe
          Success[PartialConfig](PartialConfig("one".some, "two".some, "three"))

        TestApp(Map("1" -> "one", "3" -> "three")).partialConfig mustBe
          Success[PartialConfig](PartialConfig("one".some, None, "three"))

        TestApp(Map("1" -> "one", "2" -> "two")).partialConfig mustBe
          Failure[NonEmptyList[String]](NonEmptyList("property 3 not set"))
      }
    }

    "Useful Monadic functions" in {
      import ScalaZPlayground.UsefulMonadicFunctions._

      (Some(9.some): Option[Option[Int]]).join mustBe 9.some

      List(List(1), List(2)).join mustBe List(1,2)

      List(1, 2, 3) filterM { x => List(true, false) } must contain theSameElementsAs
        List( List(1), List(2), List(3), List(1,2), List(2,3), List(1,3), List(1,2,3), Nil )

      List(1, 2) filterM { x => List(true, true) } must contain theSameElementsAs
        List( List(1,2), List(1,2), List(1,2), List(1,2) )

    }

    "Monad Transformers" in {
      import ScalaZPlayground.MonadTransformers._

      /** Ugly nesting */
      {
        for {
          o1: Option[Int] <- List(1.some, 2.some)
          o2: Option[Int] <- List(100.some)
        } yield for {
          i1: Int <- o1
          i2: Int <- o2
        } yield i1 + i2
      } mustBe List(101.some, 102.some)

      /** De-nested */
      {
        val result = for {
          i1 <- OptionT(List(1.some, 2.some))
          i2 <- OptionT(List(100.some))
        } yield i1 + i2
        result.run mustBe List(101.some, 102.some)
      }

      isDefinedOptionT mustBe List(true, true)
      isRightEitherT mustBe true.some
      isLeftEitherT mustBe false.some
    }

    "Lens" in {
      import ScalaZPlayground.Lenses._

      fromContactToNameValue.get(johnsContract) mustBe "John"
      fromContactToNameValue.set(johnsContract, "James") mustBe Contract(Person(TheName("James")))
    }

    "Memo" in {
      import ScalaZPlayground.Memorization._

      val i = 35

      val start1 = System.currentTimeMillis()
      val result1 = slowFibonacci(i)
      val end1 = System.currentTimeMillis()
      println(s"slow fibonacci($i) [millis]: " + end1.-(start1))

      val start2 = System.currentTimeMillis()
      val result2 = fastFibonacci(i)
      val end2 = System.currentTimeMillis()
      println(s"fast fibonacci($i) [millis]: " + end2.-(start2))
    }

    "IO Monad" in {
      import ScalaZPlayground.IO_Monad._

      sideEffect100.unsafePerformIO() mustBe 100
      sideEffect300.unsafePerformIO() mustBe 300
    }

  }

}
