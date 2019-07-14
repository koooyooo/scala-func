package basic

import org.scalatest.{Matchers, WordSpec}

class BasicSpec extends WordSpec with Matchers {

  "通常のメソッド" should {
    def plus(i: Int, j: Int): Int = {
      i + j // "return" は不要
    }

    "実行できる" in {
      assert(plus(1, 2) == 3)
    }

    "関数化できる" in {
      val plusVal = plus _
      assert(plusVal(1, 2) == 3)
    }

    "部分関数化できる" in {
      val curriedPlus = (plus _).curried
      val partial1 = curriedPlus(1)
      assert(partial1(2) == 3)
      assert(partial1(3) == 4)
    }

    "戻り値のない(副作用のある)メソッドは戻り値を Unit(=void)で表現できる" in {
      var result = 0
      def sideEffectPlus(i: Int, j: Int): Unit = {
        result = i + j
      }
      sideEffectPlus(1, 2)
      assert(result == 3)
    }

    "複数戻り値があるメソッドは戻り値をタプルで表現できる" in {
      def divide(i: Int, j: Int): (Int, Int) = {
        val answer = i / j
        val remainder = i % j
        (answer, remainder)
      }
      assert(divide(10, 3) == (3, 1))
    }

  }

  "ラムダ式の関数" should {

    val plus = (i: Int, j: Int) => {
      i + j
    }

    "実行できる" in {
      assert(plus(1, 2) == 3)
    }

    "関数型を明示化できる" in {
      val minus: (Int, Int) => Int = (i, j) => i - j
      assert(minus(1, 2) == -1)
    }

    "合成できる" in {
      val plusOne = (i: Int) => i + 1
      assert(plusOne(1) == 2)
      val plusTwo = plusOne compose plusOne
      assert(plusTwo(1) == 3)
    }

    "型で検証できる" in {
      type CALC[A] = (A,A) => A
      val p: CALC[Int] = plus
      assert(p(1, 2) == 3)
    }

  }
}
