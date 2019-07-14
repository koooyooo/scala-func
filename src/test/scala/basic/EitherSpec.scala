package basic

import org.scalatest.{Matchers, WordSpec}

import scala.util.{Failure, Success}

class EitherSpec extends WordSpec with Matchers {

  "値がいずれかを取ることを示すEither" should {

    "[非推奨] 単純に値を getできる" in {
      val eitherR: Either[Exception, String] = Right("Hello")
      assert(eitherR.right.get == "Hello")
      assertThrows[NoSuchElementException] {
        eitherR.left.get
      }

      val eitherL: Either[Exception, String] = Left(new Exception("Fails"))
      assertThrows[NoSuchElementException] {
        eitherL.right.get
      }
      assert(eitherL.left.get.getMessage == "Fails")
    }

    "Rightの値以外(Leftの場合)を規定値で置き換えられる" in {
      val eitherR: Either[Exception, String] = Right("Hello")
      assert(eitherR.getOrElse("") == "Hello")

      val eitherL: Either[Exception, String] = Left(new Exception("Fails"))
      assert(eitherL.getOrElse("") == "")
    }

    "事前に値の有無を確認できる" in {
      val eitherR: Either[Exception, String] = Right("Hello")
      assert(eitherR.isRight)
      assert(!eitherR.isLeft)

      val eitherL: Either[Exception, String] = Left(new Exception("Fails"))
      assert(!eitherL.isRight)
      assert(eitherL.isLeft)
    }

    "値の有無でフロー分けしながら使える" in {
      val eitherR: Either[Exception, String] = Right("Hello")
      eitherR match {
        case Right(s) => assert(s == "Hello")
        case Left(e) => fail()
      }

      val eitherL: Either[Exception, String] = Left(new Exception("Fails"))
      eitherL match {
        case Right(s) => fail()
        case Left(e) => assert(e.getMessage == "Fails")
      }
    }

    "Monadとして map処理を実施できる" in {
      val eitherR: Either[Exception, String] = Right("Hello")
      val resultR = eitherR.map(_ + " World")
      assert(resultR.right.get == "Hello World")

      // Leftの場合は map処理が適用されない
      val eitherL: Either[Exception, String] = Left(new Exception("Fails"))
      val resultL = eitherL.map(_ + " World")
      assert(resultL.left.get.getMessage == "Fails")
    }

    "左右の型定義を逆転できる" in {
      val eitherR: Either[Exception, String] = Right("Hello")
      val swapped: Either[String, Exception]= eitherR.swap

      assert(!swapped.isRight)
      assert(swapped.isLeft)
      assert(swapped.left.get == "Hello")
    }

    "Option,Seq,Tryの形に変換できる" in {
      val eitherR: Either[Exception, String] = Right("Hello")
      assert(eitherR.toOption == Some("Hello"))
      assert(eitherR.toSeq == Seq("Hello"))
      assert(eitherR.toTry == Success("Hello"))

      val eitherL: Either[Exception, String] = Left(new Exception("Fails"))
      assert(eitherL.toOption == None)
      assert(eitherL.toSeq == Seq())
      assert(eitherL.toTry == Failure(eitherL.left.get))
    }
  }

}
