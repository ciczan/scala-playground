package com.ciczan.fpbook

import scala.annotation.tailrec

object MyModule {

  def abs(n: Int): Int = {
    if (n < 0) -1 * n
    else n
  }

  private def formatAbs(x: Int): String = {
    s"The absolute value of $x is ${abs(x)}"
  }

  def factorial(n: Int): Int = {
    @tailrec
    def go(acc: Int, n: Int): Int = {
      if (n <= 0) acc
      else go(n * acc, n-1)
    }
    go(1, n)
  }

  /**
    * Fibonacci com tail recursion
    * Exercise 2.1
    */
  def fibonacci(n: Int): Int = {
    @tailrec
    def go(bef: Int, prev: Int, index: Int, fim: Int): Int = {
      if ((fim == index+1)) prev + bef
      else if (index == 0) go(0, 1, 1, fim)
      else go(prev, prev + bef, index + 1,  fim)
    }

    go(0, 0, 0, n)

  }

  /**
    * Mostra como iteirar sem break e return.
    * Basicamente vc tem um base case a mais.
    */
  def findFirst(ss: Array[String], key: String): Int = {
    @tailrec
    def go(n: Int): Int ={
      if (n >= ss.length) -1
      else if (ss(n) == key) n
      else go(n + 1)
    }

    go(0)
  }

  /**
    * Exercise 2.2
    */
  def isSorted[A](as: Array[A])(ordered: (A, A) => Boolean): Boolean = {

    def go(n: Int): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n-1), as(n))) false
      else go(n+1)
    }

    go(1)
  }

  /**
    * Exercise 2.3
    *
    * Coisa estranha mano
    */
  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    a: A => ((b: B) => f(a, b))
  }

  /**
    * Exercise 2.4
    */
  def uncurry[A, B, C](f: A => (B => C)): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /**
    * Exercise 2.5
    */
  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }

  def main(args: Array[String]): Unit = {
    println(isSorted(Array(1, 2, 3, 4, 5))(_ < _))
    println(isSorted(Array(1, 2, 3, 4, 5, 2))((a: Int, b: Int) => a < b))
  }

}
