package scalaz_playground

import cats.Semigroup
import org.scalatest.{FreeSpec, MustMatchers}

class CatsPlaygroundSpec extends FreeSpec with MustMatchers{

  "Cats Playground" - {
    "Monoid Test" in {
      import cats_playground.CatsPlayground.Monoid_Test._
      import cats.implicits._

      myCombine(List(1, 2, 3, 4)) mustBe 10
      myCombine(List("a", "b", "c")) mustBe "abc"
      myCombine(List(1.some, none, 2.some)) mustBe 3.some

      semigroupOperator(1, 2) mustBe 3
      addingIdentityToMonoid(666) mustBe 666
    }
  }
}
