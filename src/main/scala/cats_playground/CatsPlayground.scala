package cats_playground

object CatsPlayground {

  object Simple {
    // Show
    //Equal
  }


  /**
   * In "Category Theory" we have following categories :
   *
   * Magma     - type T and some binary operation (not associative)
   * Semigroup - type T + associative operation |+|
   * Monoid    - type T + associative operation + identity element (empty element)
   * Group     - type T + associative operation + identity element (empty element)
   *              + invertibility (for each element 'a: T' there is another 'b:T' for which op(a,b) == zero element
   */

  object SemiGroup_and_Monoid {

    import cats.Semigroup
    import cats.implicits._

    /**
     * Example: I can use Monoids (implemented type classes) to collaps/merge/combine all types of data
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

}