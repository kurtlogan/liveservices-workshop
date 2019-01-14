import Option._
import Either._

import scala.util.{Failure, Success, Try}

sealed trait Either[+A, +B] extends Product with Serializable { self =>

  def isLeft: Boolean

  def isRight: Boolean

  def fold[C](f1: A => C, f2: B => C): C = self match {
    case Left(value) => f1(value)
    case Right(value) => f2(value)
  }

  def swap: Either[B, A] = self match {
    case Left(value)  => Right(value)
    case Right(value) => Left(value)
  }

  def joinRight[A1 >: A, B1 >: B, C](implicit ev: B1 <:< Either[A1, C]): Either[A1, C] = self match {
    case Left(value) => Left(value)
    case Right(value) => value
  }

  def joinLeft[A1 >: A, B1 >: B, C](implicit ev: A1 <:< Either[C, B1]): Either[C, B1] = self match {
    case Right(value) => Right(value)
    case Left(value)  => value
  }

  def getOrElse[B1 >: B](orElse: B1): B1 = self match {
    case Right(value) => value
    case Left(_)      => orElse
  }

  def contains[B1 >: B](b: B1): Boolean = self match {
    case Right(value) => value == b
    case Left(_)      => false
  }

  //def forall(f: B => Boolean): Boolean

  def exists[B1 >: B](pred: B1 => Boolean): Boolean = self match {
    case Right(value) => pred(value)
    case Left(_)      => false
  }

  //def foreach[U](f: B => U): Unit

  def map[C](f: B => C): Either[A, C] = self match {
    case Right(value) => Right(f(value))
    case Left(value)  => Left(value)
  }

  def flatMap[A1 >: A, C](f: B => Either[A1, C]): Either[A1, C] = self match {
    case Right(value) => f(value)
    case Left(value)  => Left(value)
  }

  def filterOrElse[A1 >: A](pred: B => Boolean, zero: => A1): Either[A1, B] = self match {
    case Right(value) if pred(value) => Right(value)
    case _                           => Left(zero)
  }

  def toSeq: Seq[B] = self match {
    case Right(value) => Seq(value)
    case Left(_)      => Seq.empty
  }

  def toOption: Option[B] = self match {
    case Right(value) => Some(value)
    case Left(_)      => None
  }

  def toTry(implicit ev: A <:< Throwable): Try[B] = self match {
    case Right(value)   => Success(value)
    case Left(throwable) => Failure(throwable)
  }

  def left: LeftProjection[A, B] = LeftProjection(self)

  //def right: RightProjection[A, B] = RightProjection(self)
}

final case class Left[A](value: A) extends Either[A, Nothing] {

  val isLeft  = true
  val isRight = false
}

final case class Right[A](value: A) extends Either[Nothing, A] {

  val isLeft  = true
  val isRight = false
}

object Either {

  def cond[A, B](test: Boolean, right: => B, left: => A): Either[A, B] =
    if(test) Left(left) else Right(right)

  implicit class MergableEither[A](private val either: Either[A, A]) extends AnyVal {

    def merge: A = either match {
      case Left(value)  => value
      case Right(value) => value
    }
  }

  final case class LeftProjection[+A, +B](either: Either[A, B]) {

    def get: A = either match {
      case Right(_)    => throw new Exception("LeftProjection.get on Right")
      case Left(value) => value
    }

    // def foreach[U](f: A => U): Unit

    def getOrElse[A1 >: A](orElse: A1): A1 = either match {
      case Right(_)    => orElse
      case Left(value) => value
    }

    //def forall(@deprecatedName('f) p: A => Boolean): Boolean

    //def exists(@deprecatedName('f) p: A => Boolean): Boolean

    def map[C](f: A => C): Either[C, B] = either match {
      case Right(value) => Right(value)
      case Left(value)  => Left(f(value))
    }

    def flatMap[B1 >: B, C](f: A => Either[C, B1]): Either[C, B1] = either match {
      case Right(value) => Right(value)
      case Left(value)  => f(value)
    }

    def filter(pred: A => Boolean): Option[Either[A, B]] = either match {
      case Left(value) if pred(value) => Some(Left(value))
      case _                          => None
    }

    def toSeq: Seq[A] = either match {
      case Left(value) => Seq(value)
      case Right(_)    => Seq.empty
    }

    def toOption: Option[A] = either match {
      case Left(value) => Some(value)
      case Right(_)    => None
    }
  }

}