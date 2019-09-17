package com.ciczan.hackerrank

import java.io.PrintWriter

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object MakingAnagrams {
  // Complete the makeAnagram function below.
  def makeAnagram(a: String, b: String): Int = {
    //The chars we need to remove are the ones that happen in a string but not in another. We have to take care with repeated chars.
    //So basicaly what we are after is the intersection of the two strings.
    //We could use two hashmaps, but maybe we could order both strings and then remove what is not there. aaabf aabcde 4 O(n.m) O(n+m log n+m)

    val usedMap = new mutable.ListBuffer[(Char, Boolean)]()
    usedMap ++= a.toList.zip(List.fill(a.length)(false))

    var count = 0

    for (ch <- b) {
      val index = usedMap.indexOf((ch, false))
      if (index >= 0) {
        usedMap(index) = (ch, true)
      } else count += 1 //chars in smallString
    }

    count + usedMap.count(_._2 == false)
  }

  def main(args: Array[String]) {

    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val a = stdin.readLine

    val b = stdin.readLine

    val res = makeAnagram(a, b)

    printWriter.println(res)

    printWriter.close()

  }
}
