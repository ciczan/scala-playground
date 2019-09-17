package com.ciczan.fpbook

import org.scalatest.{FlatSpec, Matchers}
import com.ciczan.fpbook.List._


class Ch3Test extends FlatSpec with Matchers {


  "Init method " should " work fine" in {
    val xs = List(1, 2, 3, 4)
    List.init(xs) should be(List(1, 2, 3))
  }

  "Flatten " should " concatenate 3 lists" in {
    val list1 = List(1, 2, 3)
    val list2 = List(4, 5, 6)
    val list3 = List(7, 8, 9)

    val flat = List.flatten(list1, list2, list3)

    flat should be (List(1, 2, 3, 4, 5, 6, 7, 8, 9))

  }

  "Filter " should " remove odd elements " in {
    val input = List(1, 2, 3, 4, 5, 6, 7, 8, 9)

    filter(input)( _ % 2 == 0) should be(List(2, 4, 6, 8))
  }

  "FlatMap" should " flat all that was mapped " in {
    flatMap(List(1, 2, 4))(i => List(i, i)) should be(List(1, 1, 2, 2, 4, 4))
  }

  "AddLists" should " add two lists by element " in {
    addLists(List(1, 2, 3), List(4, 5, 6)) should be( List(5, 7, 9))
  }

  "HasSubsequence " should " contain the subsequences " in {
    val original = List(1, 2, 3, 4)

    hasSubsequence(original, original) should be(true)
    hasSubsequence(original, List(1, 2)) should be(true)
    hasSubsequence(original, List(3, 4)) should be(true)
    hasSubsequence(original, List(4)) should be(true)
    hasSubsequence(List(1), List(1, 2)) should be(false)

  }




}
