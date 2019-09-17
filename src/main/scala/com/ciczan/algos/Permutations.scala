package com.ciczan.algos

object Permutations {

  def findPermutations(input: String): Seq[String] = {

    def go(subString: String, used: Vector[Boolean]): Seq[String] =
      if (subString.length == input.length) List(subString)
      else
        (0 until input.length).withFilter(!used(_)).flatMap { c: Int =>
        go(subString + input(c), used.updated(c, true))
      }

    go("", Vector.fill(input.length)(false))
  }

  def main(args: Array[String]): Unit = {
    findPermutations("cicero").toSet.foreach(println)
  }


}