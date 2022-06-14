package playground

import cats.{Functor, Semigroup}
import org.scalatest.{FreeSpec, MustMatchers}

import scala.util.{Failure, Success, Try}

class CatsPlaygroundSpec extends FreeSpec with MustMatchers{

  "Cats Playground" - {
    "Simple stuff" in {
      import CatsPlayground.Simple._
      import cats.implicits._

      //Equal
      equalTypeSafeForPerson(Person("rob", 23), Person("mike", 23)) mustBe false

      //Show
      Car("blue").show mustBe "Car(BLUE)"
    }
    "SemiGroup and Monoid" in {
      import playground.CatsPlayground.SemiGroup_and_Monoid._
      import cats.implicits._

      myCombine(List(1, 2, 3, 4)) mustBe 10
      myCombine(List("a", "b", "c")) mustBe "abc"
      myCombine(List(1.some, none, 2.some)) mustBe 3.some

      semigroupOperator(1, 2) mustBe 3
      addingIdentityToMonoid(666) mustBe 666
    }
    "Functor" in {
      import CatsPlayground.Functor_Test._

      Functor[Option].map(Option("Hello"))(_.length) mustBe Some(5)
      Functor[Option].map(None: Option[String])(_.length) mustBe None

      mappedData mustBe List(Some(Success("ok")), None, Some(Failure(someException)))
    }
  }
}
