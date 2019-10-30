package com.ciczan.fpbook


sealed trait Option[+A] {

  /**
    * Apply f if Option is not None.
    *
    * In FP Option.map applies a function
    *
    * Exercise 4.1
    */
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  /**
    * Apply f, which may fail, to the Option is not None
    *
    * Exercise 4.1
    */
  def flatMap[B](f: A => Option[B]): Option[B] = {
    map(f) getOrElse None
  }

  /**
    * Returns op if option is None
    *
    * Exercise 4.1
    */
  def getOrElse[B >: A](op: => B): B = this match {
    case None => op
    case Some(a) => a
  }

  /**
    * Don't evaluate op unless needed
    *
    * Exercise 4.1
    */
  def orElse[B >: A](op: => Option[B]): Option[B] = {
    this.map(Some(_)).getOrElse(op)
  }


  /**
    * Convert Some to None if the value doesn't satisfy f
    */
  def filter(f: A => Boolean): Option[A] = {
    flatMap(a => if (f(a)) Some(a) else None)
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]


sealed trait Feither[+E, +A] {

  /**
    * Exercise 4.6
    */
  def map[B](f: A => B): Feither[E, B] = this match {
    case l : FLeft[E] => l
    case r: FRight[A] => FRight(f(r.value))
  }

  /**
    * Exercise 4.6
    */
  def flatMap[EE >: E, B](f: A => Feither[EE, B]): Feither[EE, B] = this match {
    case l : FLeft[E] => l
    case r: FRight[A] => f(r.value)
  }

  /**
    * Exercise 4.6
    */
  def orElse[EE >: E, B >: A](b: Feither[EE, B]): Feither[EE, B] = this match {
    case _ : FLeft[E] => b
    case r: FRight[A] => r
  }

  /**
    * Exercise 4.6
    */
  def map2[EE >: E, B, C](b: Feither[EE, B])(f: (A, B) => C): Feither[EE, C] = {
    this flatMap {aa => b.map(bb => f(aa, bb))}
  }



}
case class FLeft[+E](value: E) extends Feither[E, Nothing]
case class FRight[+A](value: A) extends Feither[Nothing, A]

object Chapter4Options {

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  /**
    * Exercise 4.2
    *
    */
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  /**
    * Extendido para compreensao
    */
  def lift[A, B](f: A => B): Option[A] => Option[B] = {
    val lifted: Option[A] => Option[B] = {oa: Option[A] =>
      oa.map(f)
    }
    lifted
  }

  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case _: Exception => None}


  def parseInsuranceRateQuote(age: String, nTickets: String): Option[Double] = {
    val ageO: Option[Int] = Try{ age.toInt }
    val ntO: Option[Int] = Try{ nTickets.toInt}
    map2(ageO, ntO)(insuranceRateQuote)
  }

  def insuranceRateQuote(age: Int, numberTickets: Int): Double = {
    //Made Up
    100 * (age * 1.2) * (numberTickets +1) * 1.1
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  /**
    * Exercise 4.4
    */
  def sequence[A](a: scala.collection.immutable.List[Option[A]]): Option[List[A]] = {
    a.foldRight(Some(Nil): Option[List[A]]) {(a, b) =>
            map2(a, b)(Cons(_, _))
    }
  }

  /**
    * Exercise 4.5
    */
  import scala.collection.immutable.{ List => CoreList}
  def traverse[A, B](a: CoreList[A])(f: A => Option[B]): Option[CoreList[B]] = a match {
    case scala.Nil => None
    case h :: t => map2(f(h), traverse(t)(f))( _ :: _)
  }

  def meanEither(xs: IndexedSeq[Double]): Feither[String, Double] = {
    if (xs.isEmpty) FLeft("List is Empty")
    else FRight(xs.sum / xs.length)
  }

  def safeDiv(x: Int, y: Int): Feither[Exception, Double] = {
    try FRight(x /y)
    catch { case e: Exception => FLeft(e)}
  }

  def TryEither[A](a: => A): Feither[Exception, A] =
    try {FRight(a)}
    catch {case e: Exception => FLeft(e)}


  def traverseE[E, A, B](as: List[A])(f: A => Feither[E, B]): Feither[E, List[B]] = as match {
    case Nil => FRight(Nil)
    case Cons(h, t) => f(h).map2(traverseE(t)(f))(Cons(_, _))
  }

  def sequence[E, A](es: List[Feither[E, A]]): Feither[E, List[A]] = {
    traverseE(es)(xw => xw)
  }

  def main(args: Array[String]) = {
    println(safeDiv(3, 0))
  }

}
