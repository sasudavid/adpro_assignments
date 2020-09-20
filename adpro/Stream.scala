// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: _____
// Group name: fprog
//
// AUTHOR1: __________
// TIME1: _____ <- how much time have you used on solving this exercise set
// AUTHOR1: David Sasu
// TIME1: > 6 hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: __________
// TIME2: _____ <- how much time have you used on solving this exercise set
// AUTHOR2: Pelgrim Ionis Ernest Charraud
// TIME2: > 6 hours <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR3: Kristinn Heiðar Freysteinsson


// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
// meant to be compiled, for example: fsc Stream.scala

package adpro

import scala.collection.generic.CanBuildFrom

sealed trait Stream[+A] {
  import Stream._

  def headOption: Option[A] =
    this match {
      case Empty => None
      case Cons(h,t) => Some(h())
    }

  def tail: Stream[A] = this match {
      case Empty => Empty
      case Cons(h,t) => t()
  }

  def foldRight[B] (z : =>B) (f: (A, =>B) => B): B = this match {
      case Empty => z
      case Cons (h,t) => f (h(), t().foldRight (z) (f)) 
      // Note 1. f can return without forcing the tail
      // Note 2. this is not tail recursive (stack-safe) It uses a lot of stack
      // if f requires to go deeply into the stream. So folds sometimes may be
      // less useful than in the strict case
    }

  // Note. foldLeft is eager; cannot be used to work with infinite streams. So
  // foldRight is more useful with streams (somewhat opposite to strict lists)
  def foldLeft[B] (z : =>B) (f :(A, =>B) =>B) :B = this match {
      case Empty => z
      case Cons (h,t) => t().foldLeft (f (h(),z)) (f)
      // Note 2. even if f does not force z, foldLeft will continue to recurse
    }
  
  //Checks to see if a particular element exists in a stream.
  def exists (p : A => Boolean) :Boolean = this match {
      case Empty => false
      case Cons (h,t) => p(h()) || t().exists (p)
      // Note 1. lazy; tail is never forced if satisfying element found this is
      // because || is non-strict
      // Note 2. this is also tail recursive (because of the special semantics
      // of ||)
    }


  // Exercise 2

  def toList: List[A] = this match {
    case Cons(h,t) => h() :: t().toList
    case _ => List()
  }
  

  // Exercise 3

  def take (n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => cons(h(), empty)
    case _ => empty
  }

  def drop (n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n-1)
    case Cons(h, t) if n == 0 => Cons(h,t)
    case Cons(h, _) if n == 1 => empty
    case _ => empty
  }

  // Exercise 4

  def takeWhile (p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

 /*  It terminates very fast with no exception thrown because the rest of the tail is never evaluated. */

  //Exercise 5
  
  def forAll (p: A => Boolean): Boolean = this match {
    case Cons(h, t) if p(h()) => true && t().forAll(p)
    case Cons(h, t) if !(p(h())) => false
    case _ => false
  }

/*  naturals.forAll (_ >=0) will forever need to inspect more of the series since it’ll never encounter an element that allows it to terminate with a definite answer (this will manifest as a stack overflow rather than an infinite loop).

  forAll and exists are fine to use on finite streams on the contrary, because we don't inspect them to inspect more of the series forever
 */

  //Exercise 6
  
  def takeWhile2 (p: A => Boolean): Stream[A] = 
    foldRight(empty[A])((h,t) => if (p(h)) cons(h,t) else empty)

  //Exercise 7

  def headOption2: Option[A] = 
    foldRight(None: Option[A])((h,t) => Some(h))

  //Exercise 8 The types of these functions are omitted as they are a part of the exercises

  /* Exercise 5.7 [Chiusano, Bjarnason 2014] */

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h,t) => cons(f(h), t))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((h,t) =>
      if (f(h)) cons(h, t)
      else t)

  def append[B>:A](s: => Stream[B]): Stream[B] = // [B>:A] is a lower type bound
    foldRight(s)((h,t) => cons(h,t))

/*     A covariance annotation is only possible if the type variable is used only in covariant positions. 
    Since type variable A/B appears as a parameter type of method append, this rule is broken. 
    With the help of a lower type bound, though, we can implement a prepend method where A only appears in covariant positions. */

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h,t) => f(h) append t)

  //Exercise 09
  //Put your answer here: It's optimal for streams because intermediate streams aren't generated, this implementation of find is incremental.
  // We don't fully instantiate the intermediate stream that results from filter, streams are "first-class loops". We're not doing more processing of the stream than necessary.
  // This incremental nature doesn't generate intermediate streams, so a transformation of it requires only enough working memory to store and transform the current element.
  // It would not be the case with some Lists. So the whole process with some Streams is more efficient.


  // Exercise 13

  def map_ = ???
  def take_ = ???
  def takeWhile_ = ???
  def zipWith_ = ???

}


case object Empty extends Stream[Nothing]
case class Cons[+A] (h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {

  def empty[A]: Stream[A] = Empty

  def cons[A] (hd: => A, tl: => Stream[A]) :Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def apply[A] (as: A*) :Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
    // Note 1: ":_*" tells Scala to treat a list as multiple params
    // Note 2: pattern matching with :: does not seem to work with Seq, so we
    //         use a generic function API of Seq


  // Exercise 1

  def from (n: Int): Stream[Int] = {
    cons(n, from(n+1))
  }

  def to (n: Int): Stream[Int] = n match {
    case 0 => Empty 
    case n => cons(n, to(n-1))
  }

  val naturals: Stream[Int] = from(1)

  //Exercise 10

  def fibs = {
  def go(f1: Int, f2: Int): Stream[Int] =
    cons(f1, go(f1, f1+f2))
  go(0,1)
  }
  // Fct go inspired from exercice 1 in section 5.2.2

  //Exercise 11

/*     def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((h,s)) => cons(h, unfold(s)(f))
      case None => empty
    } */

// unfold is a corecursive function : it PRODUCES(it coterminates) data.
  
  def unfold [A, S] (z: S) (f: S => Option[(A, S)]): Stream[A] =
    f(z).map((p: (A,S)) => cons(p._1,unfold(p._2)(f))).getOrElse(empty[A])

  // Note : Access element from tuple: Tuple elements can be accessed using an underscore syntax, 
  // method tup._i is used to access the ith element of the tuple.

  // Exercise 12

  def fibs2  = unfold((0,1)) { case (f0,f1) => Some((f0,(f1,f0+f1))) }
  def from2(a: Int) = unfold(a)(a => Some((a,a+1)))

/*   fibs != fibs2 ... We do not understand why. Can you pls explain us ?  */

}