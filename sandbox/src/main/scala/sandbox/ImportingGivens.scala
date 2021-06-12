package sandbox

object ImportingGivens {}

trait TC[A]

object A:
  given num: Int = 42
  given str: String = "hello"
  given chr: Char = '?'

  extension (n: Int)
    def inc: Int = n + 1

  given TC[Int] with {}
  given TC[String] with {}

  trait TC2[A]
  given TC2[Int] with {}

object B {
  locally {
    import A.* // wiledcard selector '*' は given を含めない
    // summon[Int]

    // https://dotty.epfl.ch/docs/reference/contextual/given-imports.html
    // > Generally, a normal wildcard selector * brings all definitions other than givens or extensions into scope
    // > whereas a given selector brings all givens (including those resulting from extensions) into scope.
    // wiledcard selector で extension も import されてる？
    1.inc
  }
  locally {
    // by-type imports は by-name imports は最後にまとめてる必要がある
    // import A.{given Int, str} // NG

    // import A.given
    import A.{str, given Int, given Char}
    summon[Int]
    summon[String]
    summon[Char]
    // summon[TC[Int]]
  }
  locally {
    import A.{given TC[?]} // TC の型クラスインスタンスのみ import される。便利。
    summon[TC[Int]]
    summon[TC[String]]
    // summon[Int]
  }
}

object C {
  // 名前しか import していないのに、given もスコープに入ってる。Scala2系から暗黙値の探索範囲が変わった？
  import A.TC2
  summon[TC2[Int]]
}
