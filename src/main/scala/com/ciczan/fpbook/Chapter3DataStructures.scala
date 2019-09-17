package com.ciczan.fpbook


sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def sum(xs: List[Int]): Int = xs match {
    case Nil => 0
    case Cons(x, xxs) => x + sum(xxs)
  }

  def product(xs: List[Double]): Double = xs match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(y, ys) => y * product(ys)
  }

  def apply[A](a: A*): List[A] = {
    if (a.isEmpty) Nil
    else Cons(a.head, apply(a.tail: _*))
  }

  /**
    * Exercise 3.2
    */
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, ys) => ys
  }

  /**
    * Exercise 3.3
    */
  def setHead[A](h: A, xs: List[A]): List[A] = xs match {
    case Nil => Cons(h, Nil)
    case Cons(_, t) => Cons(h, t)
  }

  /**
    * Exercise 3.4
    */
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, ys) => drop(ys, n-1)
    }
  }

  /**
    * Exercise 3.5
    */
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
      case Cons(x, xs) if f(x) =>  dropWhile(xs, f)
      case _ => l
  }

  /**
    * Exercise 3.6
    */
  def init[A](l: List[A]): List[A] = {
    l match {
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
      case Nil => Nil
    }
  }

  /**
    * Listing 3.2
    */
  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => f(x, foldRight(xs, z)(f))
  }

  def sumRight(xs: List[Int]): Int = {
    foldRight(xs, 0)( _ + _)
  }

  def prodRight(xs: List[Double]): Double = {
    foldRight(xs, 1.0)(_ * _)
  }

  /**
    * Exercise 3.9
    */
  def lengthRight[A](as: List[A]):Int = {
    foldRight(as, 0)((_, y) => y+1)
  }

  /**
    * Exercise 3.10
    */
  @annotation.tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(h, z))(f)
  }

  /**
    * Exercise 3.11
    */
  def sumLeft[A](l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  def prodLeft[A](l: List[Double]): Double = {
    foldLeft(l, 1.0)(_ + _)
  }

  /**
    * Exercise 3.12
    */
  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil: List[A])((a: A, acc: List[A]) => Cons(a, acc))
  }

  def foldRight2[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)(f)

  }

  /**
    * Fold Left: ((1 + 2) + 3) + 4
    * Fold Right: 1 + (2 + (3 + 4))
    */
  def foldLeftViaFoldRight[A,B](l: List[A], z: B)(f: (B,A) => B): B =
    foldRight(l, (b:B) => b)(   (a,g) => b => g(f(b,a))         )(z)


  /**
    * Exercise 3.14
    */
  def append[A](l: List[A], l2: List[A]): List[A] = {
    foldRight(l, l2)(Cons(_, _))
  }

  /**
    * Exercise 3.15
    */
  def flatten[A](seqs: List[A]*): List[A] = {
    val lists: List[List[A]] = com.ciczan.fpbook.List(seqs: _*)
    foldRight(lists, Nil: List[A])(append)
  }

  /**
    * Exercise 3.16
    */
  def add1(l: List[Int]): List[Int] = foldRight(l, Nil: List[Int])((a, b) => Cons(a + 1, b))

  /**
    * Exercise 3.17
    */
  def doubleToStr(l: List[Double]): List[String] = foldRight(l, Nil: List[String])((a, b) => Cons(a.toString, b))

  /**
    * Exercise 3.18
    */
  def map[A, B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil: List[B])((h, t) => Cons(f(h), t))

  /**
    * Exercise 3.19
    */
  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRight(l, Nil: List[A]){(h, t) =>
    if (f(h)) Cons(h, t)
    else t
  }

  /**
    * Exercise 3.20
    */
  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = foldRight(l, Nil: List[B]){(h, t) =>
    append(f(h), t)
  }

  /**
    * Exercise 3.21
    */
  def filterUsingFlatmap[A](l: List[A])(f: A => Boolean): List[A] = flatMap(l)(i => if (f(i)) List(i) else Nil)

  /**
    * Exercise 3.22
    */
  def addLists(as: List[Int], bs: List[Int]): List[Int] = (as, bs) match {
      case (Cons(ah, at), Cons(bh, bt)) => Cons(ah + bh, addLists(at, bt))
      case _ => Nil
  }

  /**
    * Exercise 3.23
    */
  def zipWith[A, B, C](as: List[A], bs: List[B])(f: (A, B) => C): List[C] = (as, bs) match {
    case (Cons(ah, at), Cons(bh, bt)) => Cons(f(ah,bh), zipWith(at, bt)(f))
    case _ => Nil
  }

  /**
    * Exercise 3.24
    */
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = (sup, sub) match {
    case (_, Nil) => true
    case (Nil, _) => false
    case (Cons(ah, at), Cons(bh, bt)) =>
      if (ah == bh) hasSubsequence(at, bt)
      else hasSubsequence(at, sub)
  }

}


sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](tree: Tree[A], b: B)(f: (A, B) => B): B = tree match {
    case Leaf(x) => f(x, b)
    case Branch(l, r) => fold(l, fold(r, b)(f))(f)
  }

  def sizeOnFold[A](tree: Tree[A]): Int = fold(tree, 0)((_, n) => 1 + n)

  def maximumOnFold(tree: Tree[Int]): Int = fold(tree, 0)(_ max _)



}

object Chapter3DataStructures {

  def main(argv: Array[String])= {

    //Exercise 3.1
    val xx = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + List.sum(t)
      case _ => 101
    }
    println("Exercise 3.1: " + xx)
  }

}
