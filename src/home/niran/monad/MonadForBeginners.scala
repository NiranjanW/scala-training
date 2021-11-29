package home.niran.monad

import scala.::
import scala.collection.immutable.Nil.splitAt
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits._
import scala.collection.mutable._

object MonadForBeginners {
  //Monad has the ability to wrap a value , and transform
  case class SafeValue[+T](private val internalValue: T) { //constructor = pure or unit
    def get: T = synchronized {
      internalValue
    }

    def transform[S](transformer: T => SafeValue[S]): SafeValue[S] = synchronized { //bind or flatMap
      transformer(internalValue)
    }

  }

  //"external API

  def giveSafeValue[T](value: T): SafeValue[T] = SafeValue(value)

  val safeString: SafeValue[String] = giveSafeValue("Scala is Awesome")
  //Extract
  val string = safeString.get

  //Transform
  val upperString = string.toUpperCase()

  //wrap
  val upperSafeString = SafeValue(upperString)

  //ETW ( extract modify and wrapitback)
  val upperSafeString2 = safeString.transform(s => SafeValue(s.toUpperCase()))

  case class Person(firstName: String, lastName: String) {
    assert(firstName != null && lastName != null)
  }

  //census API

  def getPerson(firstName: String, lastName: String): Person =
    if (firstName != null) {
      if (lastName != null) {
        Person(firstName.capitalize, lastName.capitalize)
      } else {
        null
      }
    } else {
      null
    }

  //get person better

  def getPersonBetter(firstName: String, lastName: String): Option[Person] =

    Option(firstName).flatMap { fName =>
      Option(lastName).flatMap { lName =>
        Option(Person(fName, lName))
      }
    }

  def getPersonBetter1(firstName: String, lastName: String): Option[Person] =

    for {
      fName <- Option(firstName)
      lName <- Option(lastName)
    }yield (Person(fName,lName))

  case class User(id:String)
  case class Product(ske:String , prince:Double)

  def getUser(url:String):Future[User] =Future {
    User("niran")
  }

  abstract class Mylist[T] {
    def head :T
    def tail: Mylist[T]
  }

  val alist = List (1,2,3)
  val first = alist.head
  val rest =alist.tail
  val aStringLIst = List("Hello" , "World" ,"HoeRU")
  val firstString = aStringLIst.head

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




}