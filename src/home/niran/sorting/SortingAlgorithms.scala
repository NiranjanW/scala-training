package home.niran.sorting

import scala.util.Random

object SortingAlgorithms {

  def isort ( xs  :List[Int]):List[Int]= {
    if (xs.isEmpty ) Nil
    else insert (xs.head , isort(xs.tail))


  }

  def insert ( num: Int , lst :List[Int]): List[Int] = {
    if (lst.isEmpty || num <= lst.head) num ::lst
    else lst.head::insert(num ,lst.tail)

  }

  def insert1 ( x:Int , xs:List[Int]) :List[Int] = {
    xs match {
      case Nil => List(x)
      case y::xs1 =>
        if (y >= x) x ::xs
        else y ::insert1(x, xs1)

    }
  }

  def less[T <: Comparable[T]](i :T , j:T) = i.compareTo(j) <0
  def swap[T](xs: Array[T], i:Int , j:Int) {val tmp = xs(i); xs(i) = xs(j); xs(j) =tmp}

  def insSort [T <: Comparable[T]](xs :Array[T]) {
    for {
      i <- 1 to xs.size
      j <- List.range(1,i).reverse
      if less(xs(j) , xs(j -1)) } swap(xs ,j , j-1)
  }

  def msort[T](less: (T,T) =>Boolean)
              (xs: List[T]): List[T] = {
    def merge(xs: List[T], ys: List[T]): List[T] =
      (xs, ys) match {
        case (Nil, _) => ys
        case (_, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if (less(x, y)) x :: merge(xs1, ys)
          else y :: merge(xs, ys1)
      }


    val pivot = xs.length / 2
    if (pivot == 0) xs
    else {
      val (ys, zs) = xs splitAt pivot
      merge(msort(less)(ys), msort(less)(zs))
    }
  }

  def main (args :Array[String]): Unit = {
    val r =  new Random()
    val lst = for (i <- 0 to 10 ) yield  r.nextInt()
    println(s"before sort" ,lst)
    println(s"after sort" ,msort((x:Int , y:Int) => x < y)(lst.toList))

  }


}
