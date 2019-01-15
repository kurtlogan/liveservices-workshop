import Option._
import Either._

import scala.util.Try

sealed trait Either[+A, +B] /*extends Product with Serializable*/ { self =>

  def isLeft: Boolean

  def isRight: Boolean

  def fold[C](f1: A => C, f2: B => C): C = ???

  def swap: Either[B, A] = ???

  def joinRight[A1 >: A, B1 >: B, C](implicit ev: B1 <:< Either[A1, C]): Either[A1, C] = ???

  def joinLeft[A1 >: A, B1 >: B, C](implicit ev: A1 <:< Either[C, B1]): Either[C, B1] = ???

  def getOrElse[B1 >: B](orElse: B1): B1 = ???

  def contains[B1 >: B](b: B1): Boolean = ???

  //def forall(f: B => Boolean): Boolean

  def exists[B1 >: B](pred: B1 => Boolean): Boolean = ???

  //def foreach[U](f: B => U): Unit

  def map[C](f: B => C): Either[A, C] = ???

  def flatMap[A1 >: A, C](f: B => Either[A1, C]): Either[A1, C] = ???

  def filterOrElse[A1 >: A](pred: B => Boolean, zero: => A1): Either[A1, B] = ???

  def toSeq: Seq[B] = ???

  def toOption: Option[B] = ???

  def toTry(implicit ev: A <:< Throwable): Try[B] = ???

  def left: LeftProjection[A, B] = ???

  //def right: RightProjection[A, B] = RightProjection(self)
}

object Either {

  def cond[A, B](test: Boolean, right: => B, left: => A): Either[A, B] = ???

//  implicit class MergableEither[A]() extends AnyVal {
//
//  }

  final case class LeftProjection[+A, +B](either: Either[A, B]) {

    def get: A = ???

    // def foreach[U](f: A => U): Unit

    def getOrElse[A1 >: A](orElse: A1): A1 = ???

    //def forall(@deprecatedName('f) p: A => Boolean): Boolean

    //def exists(@deprecatedName('f) p: A => Boolean): Boolean

    def map[C](f: A => C): Either[C, B] = ???

    def flatMap[B1 >: B, C](f: A => Either[C, B1]): Either[C, B1] = ???

    def filter(pred: A => Boolean): Option[Either[A, B]] = ???

    def toSeq: Seq[A] = ???

    def toOption: Option[A] = ???
  }

}