package playground



import scala.util.Try

object CatsPlayground {

  object Utils {
    val someException = new Exception("error")
  }

  object Simple {
    /*
      Some(1) - gives type Some[Int]
      1.some  - gives type Option[Int] - much better
    */
    import cats.implicits._
    val res: Option[Int] = 1.some

    /**
     Equal  === and =!=
     The compiler will scream if the types are different:

     1 == true - Scala wil just warn you that types are different
     1 === true - Cats will cause  compiler error
     */

    /*
     Equal - How it's done in CATS
     */
    case class Person(name: String, age: Int)

    trait MyEq[A] {
      def ===(a: A, b: A) : Boolean
      def =!=(a: A, b: A) : Boolean
    }

    implicit val eqPerson = new MyEq[Person] {
      def ===(a1: Person, a2: Person): Boolean = a1.name == a2.name && a1.age == a2.age
      def =!=(a1: Person, a2: Person): Boolean = a1.name != a2.name || a1.age != a2.age
    }

    implicit class MyEqOps[A](p: A)(implicit ev: MyEq[A]) {
      def ===(a: A) = ev.===(a, p)
      def =!=(a: A) = ev.=!=(a, p)
    }

    def equalTypeSafeForPerson(a: Person, b: Person): Boolean = a === b

    /**
     * Order - fails compilation if types don't match (eg Int and Double)
     *
     * compare
     * max
     */

    /**
     * Show  - gives us function show: String
     *
     * Q: why use it when we have toString ?
     * A: toString works even when we don't have it implemented - printing the adress of an instance in memory - we don't want that
     * Using .show we are sure that the way t rint it is actually implemented !
     */

    import cats.Show
    import cats.implicits._

    case class Car(color: String)

    implicit val showPerson = new Show[Car] {
      override def show(t: Car): String = s"Car(${t.color.toUpperCase})"
    }
    // ShowOps are imported from     import cats.implicits._
    Car("red").show
  }


  /**
   * In "Category Theory" we have following categories :
   *
   * Magma     - type T and some binary operation (not associative)
   * Semigroup - type T + associative operation |+|
   * Monoid    - type T + associative operation + identity element (empty element)
   * Group     - type T + associative operation + identity element (empty element)
   * + invertibility (for each element 'a: T' there is another 'b:T' for which op(a,b) == zero element
   */

  object SemiGroup_and_Monoid {

    import cats.Semigroup
    import cats.implicits._

    /**
     * Why use Monoid ? : I can use Monoids (implemented type classes) to collaps/merge/combine all types of data
     */
    trait Monoid[A] {
      def combine(a1: A, a2: A): A
      def empty: A
    }

    implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
      def combine(a: Int, b: Int): Int = a + b
      def empty: Int = 0
    }
    implicit val stringMonoid: Monoid[String] = new Monoid[String] {
      def combine(a: String, b: String): String = a + b
      def empty: String = ""
    }

    /**
     * Option[A] - can make a successful monoid
     * empty = None
     * combine - will combine the Some(A) |+| Some(A) - only if A is a SemiGroup (has a combine operation |+|)
     */

    implicit def optionMonoid[A: Semigroup]: Monoid[Option[A]] = new Monoid[Option[A]] {
      def empty: Option[A] = None

      def combine(a1: Option[A], a2: Option[A]): Option[A] = (a1, a2) match {
        case (Some(a1v), Some(a2v)) => Some(a1v |+| a2v)
        case (Some(_), None) => a1
        case (None, Some(_)) => a2
        case (None, None) => None
      }
    }


    def myCombine[A](ls: List[A])(implicit ev: Monoid[A]): A = ls.foldLeft(ev.empty)(ev.combine)

    val sumOfInts: Int = myCombine(List(1, 2, 3, 4))
    val sumOfStrings: String = myCombine(List("a", "b", "c", "d"))
    val sumOfOptionsInt: Option[Int] = myCombine(List(1.some, 2.some, none, 3.some))

    def semigroupOperator[T: Semigroup](a: T, b: T) = a |+| b

    def addingIdentityToMonoid[T: Semigroup](t: T)(implicit ev: Monoid[T]): T = t |+| ev.empty
  }

  object Functor_Test {
    import Utils._
    import scala.util.{Failure, Success, Try}

    /** Functor - something which has .map(A => B):
     *
     *  trait Functor[A] {
     *     def map[B](f: A => B): Functor[B]
     *  }
     *
     *  Endofunctors: Functors with simple map(A=>B) is actually ENDOFUNCTORs - A & B are the same category of types !
     */
    import cats._

    implicit val listFunctor = new Functor[List] {
      def map[A, B](fa: List[A])(f: A => B) = fa map f
    }
    implicit val optionFunctor: Functor[Option] = new Functor[Option] {
      def map[A, B](fa: Option[A])(f: A => B) = fa map f
    }
    implicit val tryFunctor = new Functor[Try] {
      def map[A, B](fa: Try[A])(f: A => B): Try[B] = fa map f
    }

    /**
     * Why use Functors ? They compose, so we can use it to apply MAP of complex nested structures:
     */

    val httpResponses: List[Option[Try[Int]]] = List(Some(Success(200)), None, Some(Failure(someException)))

    val mappedData = listFunctor
      .compose(optionFunctor)
      .compose(tryFunctor)
      .map(httpResponses)(code => if (code == 200) "ok" else "error")
  }

  object Applicative_Test {
    import cats.implicits._
    import cats.syntax.semigroupal._

    /**
     * Applicatives are perfect for combining all results of INDEPENDENT EFFECTS
     * Effects are being run in parallel and they are combined when we have all the results
     */

    def add4Ints(a: Int, b: Int, c: Int, d: Int) = a + b + c + d

    val res: Option[Int] = (1.some, 2.some, 3.some, 4.some) mapN add4Ints
  }

  object Traverse_Sequence {
    /**
     * Traverse is like a map, M[A].map(A => N[A]) - it applies a new type of monad on each element, and then inverts the monads inside-out
     *
     * So let's say we are DB updating a list of As ... Traverse is perfect for this:
     * List[A].traverse(A => Future[A])  gives us Future[List[A]]
     *
     * Sequence - applying identity function to traverse - just inverting the monads inside out !
     */

    import cats._, cats.syntax.all._
    import cats.Applicative
    import cats.instances.future._

    case class User(name: String)

    def updateUser(u: User): Try[User] = Try(u)
    def updateUsers(us: List[User]): Try[List[User]] = us.traverse(updateUser)

    /**
     */
    val list: List[Option[Int]] = List(Some(1), Some(2), None)

    val traversed: Option[List[Int]] = list.traverse(identity)
    // traversed: Option[List[Int]] = None

    val sequenced: Option[List[Int]] = list.sequence  //it's the same
    // sequenced: Option[List[Int]] = None
  }

  object Monad_Test {
    import cats._
    import cats.implicits._

    Monad[Option].pure(42) // Some(42)
    Monad[List].flatMap(List(1,2,3))(e => List(e, e)) // List(1,1,2,2,3,3)
  }

  //todo monad transformers OptionT, FutureT, EitherT  https://medium.com/virtuslab/meow-start-using-cats-in-your-project-right-now-91737e3b8af4

  object Monad_Transformers {
    /**
     * OptionT[Future, A] contains a value Future[Option[A]]  - so it's flattened
     *
     * now when we want to map A=>B  normally we would need to .map(_.map() )  but with OptionT we use single map()
     *
     * Why use it?
     * 1- with map/flatMap if gives immediate access to data inside nested monads:  MonadA[MonadB[_]
     * 2- many additional useful functions  (EitherT is right biased)
     */
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global
    import cats._
    import cats.implicits._
    import cats.data.OptionT
    import cats.data.EitherT
    import cats.instances.future._

    val resultFut: Future[Option[Int]] = Future.successful(Some(123))

    val mappedResult: OptionT[Future, Int] = OptionT(resultFut).map(_ * 2)

    val mappedResultValue: Future[Option[Int]] = mappedResult.value

    /** for-comprehension (as it has flatMap) */

    /**
     * EitherT[F, L, R] it contains a value F[Either[L,R]]
     *
     * EitherT is right biased (map/flatMap is applied to right)
     * It's common to have Future[Either[L,R]] so for right biased we can chang it into EitherT[Future,L,R]
     */
    val twentyEitherT: EitherT[Future, String, Int] = for {
      ten <- EitherT.pure[Future, String](10)
      twenty <- EitherT.pure[Future, String](ten * 2)
    } yield twenty
  }
}
