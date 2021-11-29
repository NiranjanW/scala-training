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







}