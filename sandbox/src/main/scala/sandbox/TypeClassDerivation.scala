package sandbox

import scala.deriving.*
import scala.compiletime.{summonInline, erasedValue}
import scala.util.chaining._

object TypeClassDerivation {
  import JsonInstances.showJson

  // derives カッコ良すぎる...
  case class User(id: Int, name: String, tags: List[String]) derives Show, JsonEncoder

  val user = User(42, "John Doe", List("foo", "bar"))
  println(user.show)
  println(user.asJson.show)
}

// ===

trait Show[A]:
  def show(a: A): String

object Show:
  inline def derived[A](using m: Mirror.Of[A]): Show[A] =
    lazy val elemInstances = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[A] => showSum(s, elemInstances)
      case p: Mirror.ProductOf[A] => showProduct(p, elemInstances)

  inline def summonAll[T <: Tuple]: List[Show[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[Show[t]] :: summonAll[ts]


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
    def show(a: String) = a

  given Show[Int] with
    def show(a: Int) = a.toString

  given [A](using s: Show[A]): Show[List[A]] with
    def show(a: List[A]) = a.map(s.show).mkString("[", ",", "]")

end Show

extension [A](a: A)(using Show[A])
  def show: String = summon[Show[A]].show(a)

// ===

enum Json:
  case JString(value: String)
  case JNumber(value: BigDecimal)
  case JArray(value: List[Json])
  case JObject(value: List[(String, Json)])
end Json

object JsonInstances:
  import Json._

  given showJson: Show[Json] with
    def show(a: Json) = a match
      case j: JString => showJString.show(j)
      case j: JNumber => showJNumber.show(j)
      case j: JArray => showJArray.show(j)
      case j: JObject => showJObject.show(j)

  given showJString: Show[JString] with
    def show(a: JString) = "\"" + a.value + "\""

  given showJNumber: Show[JNumber] with
    def show(a: JNumber) = a.value.toString

  given showJArray(using Show[Json]): Show[JArray] with
    def show(a: JArray) =
      a.value.map(summon[Show[Json]].show).mkString("[ ", ", ", " ]")

  given showJObject(using Show[Json]): Show[JObject] with
    def show(a: JObject) =
      a.value.map { case (k, v) => s""""$k": ${summon[Show[Json]].show(v)}""" }
        .mkString("{ ", ", ", " }")


trait JsonEncoder[A]:
  def encode(a: A): Json

object JsonEncoder:

  inline def derived[A](using m: Mirror.Of[A]): JsonEncoder[A] =
    lazy val elems = summonAll[m.MirroredElemTypes]
    inline m match
      case s: Mirror.SumOf[A] =>
        new JsonEncoder[A]:
          def encode(a: A): Json =
            elems(s.ordinal(a)).asInstanceOf[JsonEncoder[A]].encode(a)
      case p: Mirror.ProductOf[A] =>
        new JsonEncoder[A]:
          def encode(a: A): Json =
            val product = a.asInstanceOf[Product]
            product.productElementNames
              .zip(product.productIterator)
              .zip(elems.iterator)
              .map { case x @ ((k, v), elem) => (k, elem.asInstanceOf[JsonEncoder[Any]].encode(v))}
              .pipe(i => Json.JObject(i.toList))

  inline def summonAll[T <: Tuple]: List[JsonEncoder[_]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (t *: ts) => summonInline[JsonEncoder[t]] :: summonAll[ts]

  given JsonEncoder[Int] with
    def encode(a: Int): Json = Json.JNumber(BigDecimal(a))

  given JsonEncoder[String] with
    def encode(a: String): Json = Json.JString(a)

  given [A](using JsonEncoder[A]): JsonEncoder[List[A]] with
    def encode(a: List[A]): Json = Json.JArray(a.map(summon[JsonEncoder[A]].encode))

end JsonEncoder

extension [A](a: A)(using encoder: JsonEncoder[A])
  def asJson: Json = encoder.encode(a)
