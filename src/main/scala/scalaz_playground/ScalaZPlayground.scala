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

    def showsPerson: String = Person("Rob", 32).shows

    /**
     * Enum
     *
     * 'a' to 'c'    - NumericRange(a, b, c)
     * 'a' |-> 'c'   - List(a, b, c)
     */
  }

}

