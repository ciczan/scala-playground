package com.ciczan.algos

import org.scalatest.{FlatSpec, Matchers}

class TestCombinations  extends FlatSpec with Matchers {

  "The string XYWZ " should " have 15 combinations " in {

    val nonRecResult = Combinations.nonFP("XYWZ")

    nonRecResult.size should be(15)


  }
  "The string XYWZ " should " have 15 combinations for the FP verions" in {

    val nonRecResult = Combinations.fp2("XYWZ")

    nonRecResult.size should be(15)


  }

}
