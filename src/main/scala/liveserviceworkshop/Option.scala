import Option._

object Option {

  def apply[A](value: A): Option[A] =
    if(value == null) None else Some(value)

  def empty: Option[Nothing] = None

  final case class Some[A](value: A) extends Option[A] {
    def isEmpty = false

    def get: A = value
  }

  final case object None extends Option[Nothing] {
    def isEmpty = true

    def get: Nothing = throw new Exception("None.get")
  }
}

sealed trait Option[+A] { self =>

  def get: A

  def isEmpty: Boolean

  def isDefined: Boolean = !isEmpty
  
  def nonEmpty: Boolean = isDefined

  def getOrElse[A1 >: A](orElse: A1): A1 = self match {
    case Some(value) => value
    case None        => orElse
  }

  def map[B](f: A => B): Option[B] = self match {
    case Some(value) => Some(f(value))
    case None        => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] = self match {
    case Some(value) => f(value)
    case None        => None
  }

  def flatten[B](implicit ev: A <:< Option[B]): Option[B] = self match {
    case Some(value) => ev(value)
    case None        => None
  }

  def fold[B](ifEmpty: B)(f: A => B): B = self match {
    case Some(value) => f(value)
    case None        => ifEmpty
  }

  def filter(pred: A => Boolean): Option[A] = self match {
    case Some(value) if pred(value) => Some(value)
    case _                          => None
  }

  def filterNot(pred: A => Boolean): Option[A] = ???

  def exists(pred: A => Boolean): Boolean = self match {
    case Some(value) => pred(value)
    case None        => false
  }

  def contains[A1 >: A](value: A1): Boolean = exists(_ == value)

  def forall(pred: A => Boolean): Boolean = exists(pred)

  // def foreach[U](f: A => U): Unit

  def collect[B](pf: PartialFunction[A, B]): Option[B] = self match {
    case Some(value) => pf.lift(value) match {
      case scala.Some(value) => Some(value)
      case scala.None        => None
    }
    case None => None
  }

  def orElse[B >: A](alt: Option[B]): Option[B] = self match {
    case Some(value) => Some(value)
    case None        => alt
  }

  def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = getOrElse(ev(null))

  def toList: List[A] = self match {
    case Some(value) => List(value)
    case None        => List()
  }

  def toLeft[B](right: => B): Either[A, B] = self match {
    case Some(value) => Left(value)
    case None        => Right(right)
  }


//  def toRight[B](left: => B): Either[B, A] = self match {
//    case Some()
//  }

  def zip[B](that: Option[B]): Option[(A, B)] =
    self.flatMap(a => that.map(b => (a, b)))
}
