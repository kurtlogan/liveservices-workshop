package liveserviceworkshop

import scala.collection.immutable.Stream
import List._

import scala.annotation.tailrec
import scala.math.Ordering

// Hierarchy
// https://docs.scala-lang.org/resources/images/tour/collections-immutable-diagram.svg

sealed trait List[+A] { self =>

  def isEmpty: Boolean
  def head: A
  def tail: List[A]

  def ::[A1 >: A](value: A1): List[A1] =
    List.::(value, self)

  def :::[A1 >: A](that: List[A1]): List[A1] = that match {
    case Nil => self
    case x :: xs => x :: (xs ::: self)
  }

  // append a list
  def ++[A1 >: A](that: List[A1]): List[A1] = self match {
    case Nil => that
    case x :: xs => x :: (xs ::: that)
  }

  // prepend a list
  def ++:[A1 >: A](that: List[A1]): List[A1] = self ::: that

  // prepend to list
  def +:[A1 >: A](elem: A1): List[A1] = elem :: self

  // append to list
  def :+[A1 >: A](elem: A1): List[A1] = self ++ List(elem)

  def headOption: Option[A] = self match {
    case x :: _ => Some(x)
    case List() => None
  }

  @tailrec
  final def last: A = self match {
    case Nil      => throw new Exception("last on empty list")
    case x :: Nil => x
    case x :: xs  => xs.last
  }

  @tailrec
  final def lastOption: Option[A] = self match {
    case Nil      => None
    case x :: Nil => Some(x)
    case x :: xs  => xs.lastOption
  }

  def init: List[A] = {

    @tailrec
    def loop(xs: List[A], acc: List[A]): List[A] = xs match {
      case Nil      => Nil
      case x :: Nil => acc
      case x :: xs  => loop(xs, acc :+ x)
    }

    loop(self, Nil)
  }

  def take(n: Int): List[A] = {
    @tailrec
    def loop(xs: List[A], n: Int, acc: List[A]): List[A] =
      xs match {
        case Nil     => acc.reverse
        case x :: xs =>
          if (n <= 0) acc.reverse
          else loop(xs, n - 1, x :: acc)
      }

    loop(self, n, Nil)
  }

  def drop(n: Int): List[A] = {
    @tailrec
    def loop(n: Int, dec: List[A]): List[A] = dec match {
      case Nil => Nil
      case _ :: xs =>
        if(n <= 0) dec
        else loop(n - 1, xs)
    }

    loop(n, self)
  }

  def slice(from: Int, until: Int): List[A] = drop(from).take(until - from)

  def takeRight(n: Int): List[A] = self.reverse.take(n).reverse

  def dropRight(n: Int): List[A] = self.reverse.drop(n).reverse

  def splitAt(n: Int): (List[A], List[A]) = (self.take(n), self.drop(n))

  def map[B](f: A => B): List[B] = self match {
    case Nil => Nil
    case x :: xs => f(x) :: xs.map(f)
  }

  def collect[B](pf: PartialFunction[A, B]): List[B] = ???

  def flatMap[B](f: A => List[B]): List[B] = self match {
    case Nil     => Nil
    case x :: xs => f(x) ::: xs.flatMap(f)
  }

  def takeWhile(p: A => Boolean): List[A] = ???

  def dropWhile(p: A => Boolean): List[A] = ???

  def span(p: A => Boolean): (List[A], List[A]) = ???

  def reverse: List[A] = self match {
    case Nil     => Nil
    case x :: xs => List(x) ++ xs.reverse
  }

  def fold[A1 >: A](z: A1)(op: (A1, A1) => A1): A1 = foldLeft(z)(op)

  def foldLeft[B](z: B)(op: (B, A) => B): B = self match {
    case Nil => z
    case x :: xs => xs.foldLeft(op(z, x))(op)
  }

  def foldRight[B](z: B)(op: (A, B) => B): B =
    reverse.foldLeft(z)((l, r) => op(r, l))

  def filter(p: A => Boolean): List[A] = self match {
    case Nil => Nil
    case x :: xs =>
      if (p(x)) x :: xs.filter(p)
      else xs.filter(p)
  }

  def find(p: A => Boolean): Option[A] = self match {
    case Nil            => None
    case x :: _ if p(x) => Some(x)
    case _ :: xs        => xs.find(p)
  }

  def partition(p: A => Boolean): (List[A], List[A]) = ???

  def toStream : Stream[A] = ???

  def length: Int = ???
  def flatten: List[A] = ???
  def mkString: String = ???
  def mkString(sep: String): String = ???
  def mkString(start: String, sep: String, end: String): String = ???
  def exists(p: A => Boolean): Boolean = ???
  def contains[A1 >: A](a: A1): Boolean = ???

  def sorted[A1 >: A](implicit ord: Ordering[A1]): List[A1] = sortBy(x => identity[A1](x))

  def sortBy[B](f: A => B)(implicit ord: Ordering[B]): List[A] = self match {
    case Nil     => Nil
    case x :: xs =>
      val O = implicitly[Ordering[B]]
      val (less, more) = xs.partition(y => O.lt(f(y), f(x)))
      less.sortBy(f) ::: (x :: more.sortBy(f))
  }

  def groupBy[K](f: A => K): Map[K, List[A]] = ???

  def unzip[B, C](implicit ev: A <:< List[(B, C)]): (List[B], List[C]) = ???

  def zip[B](that: List[B]): List[(A, B)] = ???
}

object List {

  def apply[T](elements: T*): List[T] =
    if(elements.isEmpty) Nil
    else ::(elements.head, apply(elements.tail: _*))

  def concat[A](xss: List[A]*): List[A] = ???

  def fill[A](n: Int)(elem: => A): List[A] = ???

  def tabulate[A](n: Int)(f: Int => A): List[A] = ???

  def iterate[A](start: A, len: Int)(f: A => A): List[A] = ???

  def range[T: Integral](start: T, end: T): List[T] = ???

  def range[T: Integral](start: T, end: T, step: T): List[T] = ???

  def unapplySeq[A](xs: List[A]): Option[List[A]] = Some(xs)

  final case class ::[A](head: A, tail: List[A]) extends List[A] {

    val isEmpty: Boolean = false

  }

  final case object Nil extends List[Nothing] {

    val isEmpty: Boolean = true
    def head: Nothing = throw new Exception("head on empty list")
    def tail: Nothing = throw new Exception("tail on empty list")
  }
}