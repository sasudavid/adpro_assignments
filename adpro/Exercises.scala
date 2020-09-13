// Advanced Programming, A. Wąsowski, IT University of Copenhagen
//
// Group number: _____
//
// AUTHOR1: __________
// TIME1: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// AUTHOR2: __________
// TIME2: _____ <- how much time have you used on solving this exercise set
// (excluding reading the book, fetching pizza, and going out for a smoke)
//
// You should work with the file by following the associated exercise sheet
// (available in PDF from the course website).
//
// The file shall always compile and run after you are done with each exercise
// (if you do them in order).  Please compile and test frequently. Of course,
// some tests will be failing until you finish. Only hand in a solution that
// compiles and where tests pass for all parts that you finished.  The tests
// will fail for unfnished parts.  Comment such out.

package adpro

// Exercise  1

trait OrderedPoint extends scala.math.Ordered[java.awt.Point] {

  this: java.awt.Point =>

  override def compare (that: java.awt.Point): Int =  ???

}

// Try the following (and similar) tests in the repl (sbt console):
// val p = new java.awt.Point(0,1) with OrderedPoint
// val q = new java.awt.Point(0,2) with OrderedPoint
// assert(p < q)

sealed trait Tree[+A]
case class Leaf[A] (value: A) extends Tree[A]
case class Branch[A] (left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  // Exercise 2

  def size[A] (t :Tree[A]): Int = ???

  // Exercise 3

  def maximum (t: Tree[Int]): Int = ???

  // Exercise 4

  def map[A,B] (t: Tree[A]) (f: A => B): Tree[B] = ???

  // Exercise 5

  def fold[A,B] (t: Tree[A]) (f: (B,B) => B) (g: A => B): B = ???

  def size1[A] (t: Tree[A]): Int = ???

  def maximum1 (t: Tree[Int]): Int = ???

  def map1[A,B] (t: Tree[A]) (f: A=>B): Tree[B] = ???

}

sealed trait Option[+A] {

  // Exercise 6

  def map[B] (f: A=>B): Option[B] = this match {
    case Some (a) => Some(f(a))  
    case None => None
  }

  /**
   * Ignore the arrow (=>) in default's type below for now.
   * It prevents the argument "default" from being evaluated until it is needed.
   * So it is not evaluated if we process a Some object (this is 'call-by-name' 
   * and we should talk about this soon). 
   */

  def getOrElse[B >: A] (default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B] (f: A => Option[B]): Option[B] =
  map(f) getOrElse None

  def filter (p: A => Boolean): Option[A] = this match {
    case Some(a) if p(a) => this
    case _ => None
  }

}

case class Some[+A] (get: A) extends Option[A]
case object None extends Option[Nothing]

object ExercisesOption {

  // mean is implemented in Chapter 4 of the text book

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Exercise 7

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => 
    mean(xs.map(x => 
    math.pow(x - m, 2))))


/*   def variance (xs: Seq[Double]): Option[Double] = 
  for {
    x <- xs
    xm = math.pow(x - mean(xs), 2)
  } yield mean(xs) */

  // Exercise 8

  def map2[A,B,C] (ao: Option[A], bo: Option[B]) (f: (A,B) => C): Option[C] =
  for {
    a <- ao
    b <- bo
  } yield f(a, b)

/*   ao flatMap (a => bo map (b => f(a, b)))
flatMap enables the function to be option aware
So now what your map2 does? It accepts 2 instances of Option monad and a function that transform "raw" pair of types into a single new "raw" type and 
return Option of that result type. 
Logically this is similar to map but to implement it you need a flatMap. 
To call your f you need to "unpack" both ao and bo. But monad doesn't provide a way to just "unpack" raw value. 
You might want to use just map. After all map2 is logically similar to it! However, if you write
ao.map(aa =>
  bo.map(bb => f(aa,bb)))
as you might naively do, it wouldn't work as you might expect. The reason is that bo.map(bb => f(aa,bb)) returns Option[C] 
(remember, there is no standard way to "unpack" a monad) and thus your function passed to ao.map(aa => ...) returns Option[C] and 
thus the result would be Option[Option[C]]. But this exactly where flatMap comes to the rescue! It allows you "unpack" this double Option into a simple Option[C].
Explanation Source : StackOverFlow
 */

  // Exercise 9

  def sequence[A] (aos: List[Option[A]]): Option[List[A]] =
   aos.foldRight[Option[List[A]]](Some(Nil))((x,y) => map2(x,y)(_ :: _))

  // Exercise 10

  def traverse[A,B] (as: List[A]) (f :A => Option[B]): Option[List[B]] = ???

}
