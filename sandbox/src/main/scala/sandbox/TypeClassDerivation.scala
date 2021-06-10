package sandbox

import scala.deriving.*
import scala.compiletime.{summonInline, erasedValue}

object TypeClassDerivation {
  // derives カッコ良すぎる...
  case class User(id: Int, name: String) derives Show

  println(User(42, "aoiroaoino").show)
}

inline def summonAll[T <: Tuple]: List[Show[_]] =
  inline erasedValue[T] match
    case _: EmptyTuple => Nil
    case _: (t *: ts) => summonInline[Show[t]] :: summonAll[ts]

trait Show[A]:
  def show(a: A): String

object Show:
  inline def derived[A](using m: Mirror.Of[A]): Show[A] =
    lazy val elemInstances = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[A] => showSum(s, elemInstances)
      case p: Mirror.ProductOf[A] => showProduct(p, elemInstances)

  def showSum[A](s: Mirror.SumOf[A], elems: => List[Show[_]]): Show[A] =
    new Show[A]:
      def show(a: A): String = elems(s.ordinal(a)).asInstanceOf[Show[A]].show(a)

  def showProduct[A](p: Mirror.ProductOf[A], elems: => List[Show[_]]): Show[A] =
    new Show[A]:
      def show(a: A): String = {
        val product = a.asInstanceOf[Product]
        product.productPrefix +
          product
            .productIterator
            .zip(elems.iterator)
            .map { case (a, elem) => elem.asInstanceOf[Show[A]].show(a.asInstanceOf[A]) }
            .mkString("(", ", ", ")")
      }

  given Show[String] with
    def show(a: String) = a.reverse

  given Show[Int] with
    def show(a: Int) = a.toString

end Show

extension [A](a: A)(using Show[A])
  def show: String = summon[Show[A]].show(a)

