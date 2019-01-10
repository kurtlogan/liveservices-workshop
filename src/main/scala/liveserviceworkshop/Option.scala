import Option._

object Option {

  def apply[A](value: A): Option[A] = ???

  def empty: Option[Nothing] = ???

}

trait Option[A] { self =>

  def isEmpty: Boolean

  def isDefined: Boolean
  
  def nonEmpty: Boolean

  def getOrElse(orElse: A): A

  def map[B](f: A => B): Option[B]

  def flatMap[B](f: A => Option[B]): Option[B]

  def flatten[B]: Option[B]

  def fold[B](ifEmpty: B)(f: A => B)

  def filter(pred: A => Boolean): Option[A]

  def filterNot(pred: A => Boolean): Option[A]

  def exists(pred: A => Boolean): Boolean

  def contains(value: A): Boolean

  def forall(pred: A => Boolean): Boolean

  def foreach[U](f: A => U): Unit

  def collect[B](pf: PartialFunction[A, B]): Option[B]

  def orElse[B >: A](alt: Option[B]): Option[B]

  def orNull: A

  def toList: List[A]

  def toLeft[B](right: => B): Either[A, B]

  def toRight[B](left: => B): Either[B, A]
}
