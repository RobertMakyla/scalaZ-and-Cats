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
    import ScalaZPlayground.FunctorApplicativeMonad._

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

  }

}
