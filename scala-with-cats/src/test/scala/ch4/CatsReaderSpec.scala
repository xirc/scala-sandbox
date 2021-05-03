package ch4

import cats.data.Reader
import cats.syntax.all._
import testing.BaseSpec

final class CatsReaderSpec extends BaseSpec {

  "exercise" in {

    final case class Db(
        usernames: Map[Int, String],
        passwords: Map[String, String]
    )

    type DbReader[A] = Reader[Db, A]

    def findUser(userId: Int): DbReader[Option[String]] =
      Reader(db => db.usernames.get(userId))

    def checkPassword(username: String, password: String): DbReader[Boolean] =
      Reader(db => db.passwords.get(username).contains(password))

    def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
      for {
        username <- findUser(userId)
        passwordOk <- username
          .map { username =>
            checkPassword(username, password)
          }
          .getOrElse(false.pure[DbReader])
      } yield {
        passwordOk
      }
    }

    val usernames = Map(1 -> "myname")
    val passwords = Map("myname" -> "mypassword")
    val db = Db(usernames, passwords)

    checkLogin(1, "mypassword").run(db) shouldBe true
    checkLogin(2, "mypassword").run(db) shouldBe false

  }

}
