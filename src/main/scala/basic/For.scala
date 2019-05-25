package basic

import scala.util.{Failure, Success, Try}

object For {
  def main(args: Array[String]): Unit = {

    val a = for {
      i <- plus(100, 2)
      j <- plus(3, 50)
    } yield (i, j)

    println(a)
  }


  def plus(i: Int, j: Int): Try[Int] = {
    if (i != 100) Success(i + j) else Failure(new Exception("100 is error"))
  }

}
