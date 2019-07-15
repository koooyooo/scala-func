package basic

import org.scalatest.{Matchers, WordSpec}

class PartialFuncSpec extends WordSpec with Matchers {
  "Partial Function" should {

    val partialFunc: PartialFunction[Int,Int] = {
      case 1 => 2
      case 2 => 4
      case 5 => 10
    }

    "定義のある値は返る" in {
      assert(partialFunc(2) == 4)
    }

    "定義のない値はエラーとなる" in {
      assertThrows[MatchError] {
        partialFunc(3)
      }
    }

    "liftを行うことで戻り値を Option化できる" in {
      val liftedPF = partialFunc.lift

      assert(liftedPF(2) == Some(4))
      assert(liftedPF(3) == None)
    }

    "orElseを行うことで追加定義ができる" in {
      val chainedPF = partialFunc.orElse[Int,Int] {
        case 10 => 20
      }
      assert(chainedPF(2) == 4)
      assert(chainedPF(10) == 20)

      assertThrows[MatchError] {
        partialFunc(3)
      }
    }

    "mapを適用することができる" in {
      val mappedList = Seq(1, 2, 3).map {
        case 1 => 2
        case 2 => 4
        case 3 => 6
      }
      assert(mappedList == Seq(2, 4, 6))

      // 定義がない場合はエラー
      assertThrows[MatchError] {
        Seq(1, 2, 3).map {
          case 1 => 2
          case 2 => 4
          case 5 => 10
        }
      }
    }


  }
}
