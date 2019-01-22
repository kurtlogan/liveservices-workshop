package liveserviceworkshop

object OptionAnswers {

  import Option._

  sealed trait Option[+A] { // sealed abstract class Option[+A] extends Product with Serializable
    self =>

    def isEmpty: Boolean

    def isDefined: Boolean = !isEmpty

    def nonEmpty: Boolean = isDefined

    def get: A // don't do this

    final def map[B](f: A => B): Option[B] = self match {
      case Some(value) => Some(f(value))
      case None        => None
    }

    final def flatMap[B](f: A => Option[B]): Option[B] = self match {
      case Some(value) => f(value)
      case None        => None
    }

    final def flatten[B](implicit ev: A <:< Option[B]): Option[B] = self match {
      case Some(value) => value
      case None        => None
    }

    // Don't bother with @inline 9/10 the jit will perform the optimisation if it is required
    @inline final def getOrElse[A1 >: A](orElse: A1): A1 = self match {
      case Some(value) => value
      case None        => orElse
    }

    final def fold[B](ifEmpty: B)(f: A => B): B = self match {
      case Some(value) => f(value)
      case None        => ifEmpty
    }

    final def filter(pred: A => Boolean): Option[A] = self match {
      case Some(value) if pred(value) => Some(value)
      case None                       => None
    }

    final def filterNot(pred: A => Boolean): Option[A] = self match {
      case Some(value) if !pred(value) => Some(value)
      case None                        => None
    }

    final def contains[A1 >: A](value: A1): Boolean = exists(_ == value)

    final def exists(pred: A => Boolean): Boolean = self match {
      case Some(value) => pred(value)
      case None        => false
    }

    final def forall(pred: A => Boolean): Boolean = exists(pred)

    final def foreach[U](pred: A => U): Unit = self match {
      case Some(value) => pred(value)
      case None        => ()
    }

    final def collect[B](pf: PartialFunction[A, B]): Option[B] = self match {
      case Some(value) => pf.lift(value) match {
        case scala.Some(value) => Some(value)
        case scala.None        => None
      }
      case None        => None
    }

    final def orElse[B >: A](alt: Option[B]): Option[B] = self match {
      case Some(value) => Some(value)
      case None        => alt
    }

    final def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = getOrElse(ev(null))

//    final def toList: List[A] = self match {
//      case Some(value) => List(value)
//      case None        => Nil
//    }

//    final def toLeft[B](right: => B): Either[A, B] = self match {
//      case Some(value) => Left(value)
//      case None        => Right(right)
//    }
//
//    final def toRight[B](left: => B): Either[B, A] = self match {
//      case Some(value) => Right(value)
//      case None        => Left(left)
//    }
  }

  object Option {

    final case class Some[A](value: A) extends Option[A] {
      override def isEmpty: Boolean = false

      override def get: A = ???
    }

    final case object None extends Option[Nothing] {
      override def isEmpty: Boolean = true

      override def get: Nothing = ???
    }

    def apply[A](value: A): Option[A] = if(value == null) None else Some(value)

    def empty: Option[Nothing] = None
  }
}