package basic

import scala.util.Try

trait Module {
  val func1: Int => Try[Int] = { i =>
    func2(i).flatMap(func3).toTry
  }
  val func2: Int => Either[ExceptionA, Int] = { i =>
    if (i < 0) Left(new ExceptionA("negative value")) else Right(i * 2)
  }
  val func3: Int => Either[ExceptionB, Int] = { i =>
    if (i < 0) Left(new ExceptionB("negative value")) else Right(i * 3)
  }
}
class ExceptionA(msg: String) extends Exception(msg)
class ExceptionB(msg: String) extends Exception(msg)

object Main extends Module {
  def main (args: Array[String] ): Unit = {
    println(func1(1))  // Success(6)
    println(func1(-1)) // Failure(basic.ExceptionA: negative value)
  }
}
