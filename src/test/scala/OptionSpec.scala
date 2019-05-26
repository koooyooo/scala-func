import org.scalatest.{Matchers, WordSpec}

class OptionSpec extends WordSpec with Matchers {
  "値の有無 双方の可能性を表す Option" should {

    "複数の生成をサポートしている" in {
      // Option内で判定を行うもの
      val opt1 = Option(1)
      val opt2 = Option(null)
      val opt3 = Option{
        1 + 1
      }

      // 明示的に結果を用意するもの
      val opt4 = Some(3)
      val opt5 = None

      assert(opt1.isDefined)
      assert(!opt2.isDefined)
      assert(opt3.isDefined)
      assert(opt4.isDefined)
      assert(!opt5.isDefined)
    }

    "[非推奨] 単純に値を getできる" in {
      val opt1: Option[Int] = Some(1)
      assert(opt1.get == 1)

      // Noneの場合に例外が発生してしまい Optionで保護する意味が薄れるので非推奨
      val opt2: Option[Int] = None
      assertThrows[NoSuchElementException] {
        opt2.get
      }
    }

    "Noneの場合を規定値として表現できる" in {
      val opt1: Option[Int] = Some(1)
      val opt1Result = opt1.getOrElse(100)
      assert(opt1Result == 1)

      val opt2: Option[Int] = None
      val opt2Result = opt2.getOrElse(100)
      assert(opt2Result == 100)
    }

    "Java的に既定値を nullとして操作できる" in {
      val opt1: Option[Int] = Some(1)
      assertTypeError("opt1.orNull == 1") // Javaの intに nullは代入できないのでタイプエラー

      val opt2: Option[Int] = None
      assertTypeError("opt2.orNull == null") // Javaの intに nullは代入できないのでタイプエラー

      val strOpt1: Option[String] = Some("some")
      assert(strOpt1.orNull == "some")


      val strOpt2: Option[String] = None
      assert(strOpt2.orNull == null)
    }

   "Noneの場合の既定値をOption型でも表現できる" in {
      val opt1: Option[Int] = Some(1)
      val opt1Result = opt1.orElse(Some(2))
      assert(opt1Result == Some(1))

      val opt2: Option[Int] = None
      val opt2Result = opt2.orElse(Some(2))
      assert(opt2Result == Some(2))
    }

    "事前に値の有無を確認できる" in {
      val opt1: Option[Int] = Some(1)
      assert(!opt1.isEmpty)
      assert(opt1.nonEmpty)

      val opt2: Option[Int] = None
      assert(opt2.isEmpty)
      assert(!opt2.nonEmpty)
    }

    "値の有無でフロー分けしながら使える" in {
      val opt1: Option[Int] = Some(1)
      val result = opt1 match {
        case Some(v) => v
        case None => 0
      }
      assert(result == 1)
    }

    "Monadとして flatMap経由で使える" in {
      // 元の値が全て Someなら Someの計算値が返る
      val opt1: Option[Int] = Some(1)
      val opt2: Option[Int] = Some(2)
      val opt3: Option[Int] = Some(3)

      val result = opt1.flatMap{ v1 =>
        opt2.flatMap{ v2 =>
          opt3.flatMap{ v3 =>
            Some(v1 + v2 + v3)
          }
        }
      }
      assert(result == Some(6))

      // 元の値に Noneが含まれるなら Noneが返る
      val optE1: Option[Int] = Some(1)
      val optE2: Option[Int] = None
      val optE3: Option[Int] = Some(3)

      val resultE = optE1.flatMap{ v1 =>
        optE2.flatMap{ v2 =>
          optE3.flatMap{ v3 =>
            Some(v1 + v2 + v3)
          }
        }
      }
      assert(resultE == None)
    }

    "Monadとして for文が使える" in {
      val opt1: Option[Int] = Some(1)
      val opt2: Option[Int] = Some(2)
      val opt3: Option[Int] = Some(3)

      val result = for {
        r1 <- opt1
        r2 <- opt2
        r3 <- opt3
      } yield r1 + r2 + r3

      assert(result == Some(6))


      val optE1: Option[Int] = Some(1)
      val optE2: Option[Int] = None
      val optE3: Option[Int] = Some(3)

      val resultE = for {
        r1 <- optE1
        r2 <- optE2
        r3 <- optE3
      } yield r1 + r2 + r3

      assert(resultE == None)
    }

    "Mapを用いた計算も可能" in {
       // Mapは計算後に Someで括るので同じ使い方をすると大変なことになる
      val opt1: Option[Int] = Some(1)
      val opt2: Option[Int] = Some(2)
      val opt3: Option[Int] = Some(3)

      val result = opt1.map{ v1 =>
        opt2.map{ v2 =>
          opt3.map{ v3 =>
            v1 + v2 + v3
          }
        }
      }
      assert(result == Some(Some(Some(6))))

      // mapは Monadに 非Monadを結合する場合に使う
      val result2 = Option[Int](1).map(_ + 2).map(_ + 3)
      assert(result2 == Some(6))

      // 尚 Int型では nullに対し結合すると Type Error
      // assertTypeError("Option[Int](null).map(_ + 2).map(_ + 3)")


      val result3 = Option[String]("one").map(_ + "two").map(_ + "three")
      assert(result3 == Some("onetwothree"))

      val result4 = Option[String](null).map(_ + "two").map(_ + "three")
      assert(result4 == None)
    }

  }
}
