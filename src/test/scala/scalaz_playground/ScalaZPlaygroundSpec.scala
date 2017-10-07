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
      notEqualTypeSafeForInts(1, 1) mustBe false

      equalTypeSafeForPerson(Person("Rob", 32), Person("Mon", 31)) mustBe false
      notEqualTypeSafeForPerson(Person("Rob", 32), Person("Mon", 31)) mustBe true
    }

    "Ordering" in {
      isBiggerTypeSafe(100, 200) mustBe false
    }

    "Show: Cord / Shows: String " in {
      Person("Rob", 33).shows mustBe "name: Rob, age: 33"
    }
  }

  "Functor, Monad, Applicative" - {
    import ScalaZPlayground.FunctorMonadApplicative._

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

  }

}
