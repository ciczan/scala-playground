package com.ciczan.algos

import org.scalatest._
import com.ciczan.algos.BreadthFirst._


class TestBreadthFirst extends FlatSpec with Matchers {

  val graph = new Graph[String]()

  graph.addEdges("You", List("Claire", "Bob", "Alice"):_*)
  graph.addEdges("Claire", List("Thom", "Jonny"):_*)
  graph.addEdges("Alice", List("Peggy"):_*)
  graph.addEdges("Bob", List("Peggy", "Anuj"):_*)

  "A Breadth First Search " should " return and Option with the closest node that passes the predicate " in {

    val result = search("You", graph, (n: String) => n.endsWith("m"))

    result should be (Option("Thom"))
  }

  "A Breadth First Search " should " return an empty Option if there is no node that meets the criteria " in {

    val result = search("You", graph, (n: String) => n.endsWith("mm"))

    result should be (Option.empty)
  }

  "A BFS with depth " should " return and Option with the closest node and its depth " in {

    val result = searchDepth("You", graph, (n: String) => n.endsWith("m"))

    result should be (Option(2, "Thom"))
  }

  "A BFS with depth " should " return an empty Option if there is no node that meets the criteria " in {

    val result = searchDepth("You", graph, (n: String) => n.endsWith("mm"))

    result should be (Option.empty)
  }

}
