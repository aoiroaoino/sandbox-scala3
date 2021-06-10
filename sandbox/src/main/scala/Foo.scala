case class Foo(i: Int, j: String)

object Main:
  def foo = scala.compiletime.codeOf(Foo(1, "aaa"))
  1 / 0


