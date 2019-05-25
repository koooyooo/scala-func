import org.scalatest._

import scala.util.{Failure, Success, Try}

class ForSpec extends WordSpec with Matchers {

  /** 正の数の和を返す関数を定義 */
  def positivePlus(i: Int, j: Int): Try[Int] = {
    if (i >= 0) Success(i + j) else Failure(new IllegalArgumentException("negative value"))
  }

  /** 正の数の和を返す関数を2回コールする For文 */
  def forStatement(i: (Int, Int), j: (Int, Int)): Try[Int] = {
    for {
      i <- positivePlus(i._1, i._2)
      j <- positivePlus(j._1, j._2)
    } yield i + j
  }

  /** For文と同等のflatMap */
  def flatMapStatement(i: (Int, Int), j: (Int, Int)): Try[Int] = {
    positivePlus(i._1, i._2).flatMap{ i =>
      positivePlus(j._1, j._2).flatMap{ j =>
        Success(i + j)
      }
    }
  }

  "Forループの挙動" should {

    "成功時は Successが返る" in {
      assert(forStatement((10, 20), (30, 40)) == Success(100))
    }

    "失敗時は 処理が中断され Failureが返る" in {
      // 前段の処理が失敗するように設定
      val result = forStatement((-10, 20), (30, 40))
      assert(result.isFailure)
    }

    "失敗時の Failureには Exceptionを継承した要素が含まれる" in {
      val result = forStatement((-10, 20), (30, 40))
      val failure = result.asInstanceOf[Failure[Exception]]
      // failure.getで例外を取得するとその場でthrowされるため failure.exceptionを用いる
      val exception = failure.exception
      assert(exception.isInstanceOf[IllegalArgumentException])
      assert(exception.getMessage == "negative value")
    }
  }

  "FlatMapの挙動" should {

    "成功時は Successが返る" in {
      assert(flatMapStatement((10, 20), (30, 40)) == Success(100))
    }

    "失敗時は 処理が中断され Failureが返る" in {
      // 前段の処理が失敗するように設定
      val result = flatMapStatement((-10, 20), (30, 40))
      assert(result.isFailure)
    }

    "失敗時の Failureには Exceptionを継承した要素が含まれる" in {
      val result = flatMapStatement((-10, 20), (30, 40))
      val failure = result.asInstanceOf[Failure[Exception]]
      // failure.getで例外を取得するとその場でthrowされるため failure.exceptionを用いる
      val exception = failure.exception
      assert(exception.isInstanceOf[IllegalArgumentException])
      assert(exception.getMessage == "negative value")
    }
  }
}
