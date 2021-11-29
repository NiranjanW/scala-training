package home.niran.typeclass

object component {

  def processMyList[T](list: List[T])(implicit summable: Summable[T]): T = { //ad-hoc polymorphisim
    //sum list elements
    summable.sumElements(list)
  }

  trait Summable[T] {
    def sumElements(list: List[T]): T
  }

  implicit object IntSummable extends Summable[Int] {
    override def sumElements(list: List[Int]): Int = list.sum
  }

  implicit object StringSummable extends Summable[String] {
    override def sumElements(list: List[String]): String = list.mkString(" ")
  }


  def main(args: Array[String]): Unit = {
    val intSumm = processMyList((List(1, 2, 3, 4, 5)))
    val stringSum = processMyList(List("Niran", "Wijey"))
    println(intSumm)
    println(stringSum)
  }
}
