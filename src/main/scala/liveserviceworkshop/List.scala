package liveserviceworkshop

import scala.collection.immutable.Stream
import List._

import scala.annotation.tailrec
import scala.math.Ordering

// git@github.com:kurtlogan/liveservices-workshop

// Hierarchy
// https://docs.scala-lang.org/resources/images/tour/collections-immutable-diagram.svg

sealed trait List[+A] { self =>

  def isEmpty: Boolean

  def head: A

  def tail: List[A]

  def headOption: Option[A] = self match {
    case List() => None
    case x :: _ => Some(x)
  }

  @tailrec
  final def last: A = self match {
    case Nil      => throw new Exception("Last on Nil")
    case x :: Nil => x
    case x :: xs  => xs.last
  }

  @tailrec
  final def lastOption: Option[A] = self match {
    case Nil => None
    case x :: Nil => Some(x)
    case x :: xs  => xs.lastOption
  }

  def init: List[A] = self.take(self.length - 1)

  // prepend to start of list
  def ::[A1 >: A](value: A1): List[A1] =
    List.::(value, self)

  // prepend to list
  def :::[B >: A](that: List[B]): List[B] = self match {
    case Nil     => that
    case x :: xs => x :: (that ::: xs)
  }

  // append a list
  def ++[A1 >: A](that: List[A1]): List[A1] = self match {
    case Nil     => that
    case x :: xs => x :: (xs ++ that)
  }

  // prepend a list
  def ++:[A1 >: A](that: List[A1]): List[A1] = that ::: self

  // prepend to list
  def +:[A1 >: A](elem: A1): List[A1] = elem :: self

  // append to list
  def :+[A1 >: A](elem: A1): List[A1] = self ++ List(elem)

  def take(n: Int): List[A] = {
    @tailrec
    def loop(xs: List[A], curr: Int, acc: List[A]): List[A] = xs match {
      case Nil => acc
      case y :: ys =>
        if (curr <= 0) acc
        else loop(ys, curr - 1, acc :+ y)
    }

    loop(self, n, Nil)
  }

  def drop(n: Int): List[A] = {
    @tailrec
    def loop(curr: Int, dec: List[A]): List[A] = dec match {
      case Nil => dec
      case _ :: xs =>
        if (curr <= 0) dec
        else loop(curr - 1, xs)
    }

    loop(n, self)
  }

  def slice(from: Int, until: Int): List[A] = ???

  def takeRight(n: Int): List[A] = ???

  def dropRight(n: Int): List[A] = ???

  def splitAt(n: Int): (List[A], List[A]) = ???

  def map[B](f: A => B): List[B] = ???

  def collect[B](pf: PartialFunction[A, B]): List[B] = ???

  def flatMap[B](f: A => List[B]): List[B] = ???

  def takeWhile(p: A => Boolean): List[A] = ???

  def dropWhile(p: A => Boolean): List[A] = ???

  def span(p: A => Boolean): (List[A], List[A]) = ???

  def reverse: List[A] = ???

  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = ???

  def foldLeft[B](z: B)(op: (B, A) => B): B = ???

  def foldRight[B](z: B)(op: (A, B) => B): B = ???

  def filter(p: A => Boolean): List[A] = ???

  def find(p: A => Boolean): Option[A] = ???

  def partition(p: A => Boolean): (List[A], List[A]) = ???

  def toStream : Stream[A] = ???

  def length: Int = ???

  def flatten: List[A] = ???

  def mkString: String = ???

  def mkString(sep: String): String = ???

  def mkString(start: String, sep: String, end: String): String = ???

  def exists(p: A => Boolean): Boolean = ???

  def contains[A1 >: A](a: A1): Boolean = ???

  def sorted[A1 >: A](implicit ord: Ordering[A1]): List[A1] = ???

  def sortBy[B](f: A => B)(implicit ord: Ordering[B]): List[A] = ???

  def groupBy[K](f: A => K): Map[K, List[A]] = ???

  def unzip[B, C](implicit ev: A <:< List[(B, C)]): (List[B], List[C]) = ???

  def zip[B](that: List[B]): List[(A, B)] = ???

  def zipWithIndex: List[(Int, A)] = ???
}

object List {

  def apply[A](xs: A*): List[A] =
    if(xs.isEmpty) Nil
    else ::(xs.head, apply(xs.tail: _*))

  def unapplySeq[A](xs: List[A]): Option[List[A]] = Some(xs)

  def concat[A](xss: List[A]*): List[A] = ???

  def fill[A](n: Int)(elem: => A): List[A] = ???

  def tabulate[A](n: Int)(f: Int => A): List[A] = ???

  def iterate[A](start: A, len: Int)(f: A => A): List[A] = ???

  def range[T: Integral](start: T, end: T): List[T] = ???

  def range[T: Integral](start: T, end: T, step: T): List[T] = ???

  final case class ::[A](head: A, tail: List[A]) extends List[A] {
    override def isEmpty: Boolean = false
  }

  final object Nil extends List[Nothing] {
    override def isEmpty: Boolean = true

    override def head: Nothing = throw new Exception("head on Nil")

    override def tail: List[Nothing] = throw new Exception("tail on Nil")
  }
}