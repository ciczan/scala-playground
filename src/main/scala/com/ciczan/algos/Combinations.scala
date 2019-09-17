package com.ciczan.algos

import scala.collection.mutable.ListBuffer

object Combinations {

  /**
    * Solucao do Cracking the Code interview
    * @param in
    * @return
    */
  def nonFP(in: String): Seq[String] = {

    val sb: StringBuilder = new StringBuilder()
    val combos = new ListBuffer[String]()

    def loop(start: Int): Unit = {
      for (ii <- start until in.length) {
        sb.append(in(ii))
        combos += sb.toString()
        if (start < in.length - 1)
          loop(ii + 1)
        sb.takeRight(1)
      }
    }

    loop(0)

    combos
  }

  def fp(in: String): Seq[String] = {


    val combos = new ListBuffer[String]()



    def loop(start: Int, pre: String): Unit = {

      for (ii <- start until in.length) {
        val cb = pre + in(ii)
        combos += cb
        if (start < in.length - 1)
          loop(ii + 1, cb)
      }
    }

    loop(0, "")

    combos
  }

  def fp2(in: String): Seq[String] = {

    def go(start: Int, pre: String): List[String] = {
      pre :: in.drop(start).toList.zipWithIndex.flatMap{case (cc, ii) =>
        go(start + ii + 1, pre + cc.toString)
      }
    }
    go(0, "").tail
  }

}
