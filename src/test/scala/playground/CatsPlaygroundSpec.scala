package playground

import cats.Functor
import org.scalatest.{FreeSpec, MustMatchers}

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}
import org.scalatest.concurrent.ScalaFutures

class CatsPlaygroundSpec extends FreeSpec with MustMatchers with ScalaFutures {

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
      import CatsPlayground.Utils._

      Functor[Option].map(Option("Hello"))(_.length) mustBe Some(5)
      Functor[Option].map(None: Option[String])(_.length) mustBe None

      mappedData mustBe List(Some(Success("ok")), None, Some(Failure(someException)))
    }
    "Applicatives" in {
      import CatsPlayground.Applicative_Test._
      import CatsPlayground.Utils._

      import cats.syntax.semigroupal._
      import cats.implicits._
      import scala.concurrent.ExecutionContext.Implicits.global

      //happy path
      def add4Ints(a: Int, b: Int, c: Int, d: Int) = a + b + c + d

      (1.some, 2.some, 3.some, 4.some ).mapN(add4Ints) mustBe Some(10)
      (Future.successful(1), Future.successful(1) ).mapN(_ + _).futureValue mustBe 2
    }
    "Traverse, Sequence" in {
      import CatsPlayground.Traverse_Sequence._
      import CatsPlayground.Utils._
      import cats._, cats.syntax.all._
      import cats.Applicative
      import cats.instances.future._

      val users = List(User("a"), User("b"), User("c"))
      updateUsers(users) mustBe Success(users)
    }
    "Monads" in {
      import cats._
      import cats.implicits._

      Monad[Option].pure(42) mustBe Some(42)
      Monad[List].flatMap(List(1,2,3))(e => List(e, e)) mustBe List(1,1,2,2,3,3)
    }
  }
}
