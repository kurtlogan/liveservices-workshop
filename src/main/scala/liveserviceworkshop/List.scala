package liveserviceworkshop

import scala.collection.immutable.Stream
import List._

import scala.math.Ordering

// Hierarchy
// https://docs.scala-lang.org/resources/images/tour/collections-immutable-diagram.svg

sealed trait List[+A] { self =>

  def isEmpty: Boolean

  def head: A

  def headOption: Option[A] = ???

  def tail: List[A]

  def last: A = ???

  def lastOption: Option[A] = ???

  def init: List[A] = ???

  // prepend to start of list
  def ::[A1 >: A](value: A1): List[A1] = ???

  // prepend to list
  def :::[B >: A](prefix: List[B]): List[B] = ???

  // append a list
  def ++[A1 >: A](that: List[A1]): List[A1] = ???

  // prepend a list
  def ++:[A1 >: A](that: List[A1]): List[A1] = ???

  // prepend to list
  def +:[A1 >: A](elem: A1): List[A1] = ???

  // append to list
  def :+[A1 >: A](elem: A1): List[A1] = ???

  def take(n: Int): List[A] = ???

  def drop(n: Int): List[A] = ???

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
}

object List {

  def apply[A](xs: A*): List[A] = ???

  def unapplySeq[A](xs: List[A]): Option[List[A]] = ???

  def concat[A](xss: List[A]*): List[A] = ???

  def fill[A](n: Int)(elem: => A): List[A] = ???

  def tabulate[A](n: Int)(f: Int => A): List[A] = ???

  def iterate[A](start: A, len: Int)(f: A => A): List[A] = ???

  def range[T: Integral](start: T, end: T): List[T] = ???

  def range[T: Integral](start: T, end: T, step: T): List[T] = ???
}