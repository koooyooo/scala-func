package business

import org.scalatest.{Matchers, WordSpec}

import scala.util.{Failure, Success, Try}

class BasicSpec extends WordSpec with Matchers {

  // モデルは case class として作成することで処理を簡易化
  case class UserAccount(val id: String, val pass: String, login: Boolean = false)

  // ビジネス関数をModuleとしてTrait上で管理
  trait UserAccountModule {
    def login(id: String, pass: String): Try[UserAccount] = Try(UserAccount(id, pass, true))

    def changePass(account: UserAccount, oldPass: String, newPass: String): Try[UserAccount] =
      oldPass match {
        // copyを利用することで一部属性のみ変更できる
        case account.pass => Success(account.copy(pass = newPass))
        case _ => Failure(new Exception("password does not match"))
      }

    def logOut(account: UserAccount): Try[UserAccount] = Try {
      account.copy(login = false)
    }
  }

  trait Transactionable {
    def tx[A](op: () => Try[A]): Try[A] = {
      println("startTx")
      op() match {
        case Success(a) => {
          println("commitTx")
          Success(a)
        }
        case Failure(e) => {
          println("rollbackTx")
          Failure(e)
        }
      }
    }
  }

  // Objectとして実体化
  trait UserAccountTxModule extends UserAccountModule with Transactionable {
    def loginTx(id: String, pass: String) = tx{ () =>
      login(id, pass)
    }

  }

  object UserAccountService extends UserAccountTxModule

  "UserAccount" should {

    "パイプラインとして扱うことができる" in {
      val result =
        for {
          a <- UserAccountService.login("my-id", "my-pass")
          p <- UserAccountService.changePass(a, "my-pass", "my-pass-new")
          o <- UserAccountService.logOut(p)
        } yield (a, p, o)

      assert(result.isSuccess)

      result match {
        case Success(a) => {
          assert(a._1.id == "my-id")
          assert(a._1.pass == "my-pass")
          assert(a._1.login == true)

          assert(a._2.id == "my-id")
          assert(a._2.pass == "my-pass-new")
          assert(a._2.login == true)

          assert(a._3.id == "my-id")
          assert(a._3.pass == "my-pass-new")
          assert(a._3.login == false)
        }
        case Failure(e) => fail()
      }
    }

    "パイプライン失敗時には処理キャンセルが走る" in {
      val result =
        for {
          a <- UserAccountService.login("my-id", "my-pass")
          // パスワードの照合に失敗
          p <- UserAccountService.changePass(a, "wrong-pass", "my-pass-new")
          o <- UserAccountService.logOut(p)
        } yield (a, p, o)

      assert(result.isFailure)

      result match {
        case Success(a) => fail()
        case Failure(e) => {
          assert(e.isInstanceOf[Exception])
          assert(e.getMessage == "password does not match")
        }
      }
    }

  }


}
