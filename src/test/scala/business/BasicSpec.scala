package business

import org.scalatest.{Matchers, WordSpec}

import scala.util.{Failure, Success, Try}

class BasicSpec extends WordSpec with Matchers {

  case class UserAccount(val id: String, val pass: String, login: Boolean = false)

  trait UserAccountModule {
    def login(id: String, pass: String): Try[UserAccount] = Try(UserAccount(id, pass, true))

    def changePass(account: UserAccount, oldPass: String, newPass: String): Try[UserAccount] =
      oldPass match {
        case account.pass => Success(account.copy(pass = newPass))
        case _ => Failure(new Exception("password not match"))
      }

    def logOut(account: UserAccount): Try[UserAccount] = Try {
      account.copy(login = false)
    }
  }

  object UserAccountService extends UserAccountModule

  "UserAccount" should {
    "" in {
      val account = UserAccountService.login("my-id", "my-pass")
      assert(account.isSuccess)
      account.foreach{ u =>
        assert(u.id == "my-id")
        assert(u.pass == "my-pass")
      }
    }
  }

}
