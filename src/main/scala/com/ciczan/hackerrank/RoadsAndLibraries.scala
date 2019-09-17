package com.ciczan.hackerrank

import java.io.PrintWriter

import com.ciczan.algos.Graph

/**
  * Challenge from:
  * https://www.hackerrank.com/challenges/torque-and-development/problem?h_l=interview&playlist_slugs%5B%5D=interview-preparation-kit&playlist_slugs%5B%5D=graphs
  */
object RoadsAndLibraries {

  class Graph[A] {
    private val map = new scala.collection.mutable.HashMap[A, List[A]]()

    def vertices = map.keys

    def neighbors(node: A): Seq[A] = map.getOrElse(node, Nil)

    def addVertex(node: A): Unit = addEdges(node)

    def addEdges(node: A, neighbors: A*): Unit = {
      val ngs = map.getOrElse(node, Nil)
      map update (node, ngs ++ neighbors)
    }

    def addBiEdge(node1: A, node2: A): Unit = {
      this addEdges (node1, node2)
      this addEdges (node2, node1)
    }
  }


  /**
    *
    * @param n Number of cities
    * @param c_lib the cost to build a library
    * @param c_road the cost to repair a road
    * @param cities 2D array of integers where each contains two integers that represent cities connected by an obstructed road
    * @return total cost
    */
  def roadsAndLibraries(n: Long, c_lib: Long, c_road: Long, cities: Array[Array[Long]]): Long =  {

    if (c_road >= c_lib)
      n * c_lib
    else {

      val graph = new Graph[Long]()
      for (city <- 1L to n) graph.addVertex(city)
      for (road <- cities) graph.addBiEdge(road(0), road(1))

      import scala.collection._

      def go(node: Long, visited: mutable.Set[Long]): (Long, mutable.Set[Long]) = {
        var count = 0L
        var newVisited = visited
        graph.neighbors(node).foreach{ng =>
          if (!visited.contains(ng)) {
            newVisited += ng
            val (roads, _) = go(ng, newVisited)
            count += roads + 1
          }
        }
        (count, newVisited)
      }

      val (finalCost, _) = graph.vertices.foldLeft((0L, mutable.Set.empty[Long])) { (tup, node) =>
        val (cost, visited) = tup

        if (visited.contains(node))
          tup
        else {
          visited += node
          val (roads, newVisited) = go(node, visited)
          (cost + c_lib.toLong + (c_road.toLong*roads), newVisited)
        }

      }

      finalCost
    }
  }


  def main(args: Array[String]) {
    val stdin = scala.io.StdIn

    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))

    val q = stdin.readLine.trim.toInt

    for (qItr <- 1 to q) {
      val nmC_libC_road = stdin.readLine.split(" ")

      val n = nmC_libC_road(0).trim.toLong

      val m = nmC_libC_road(1).trim.toLong

      val c_lib = nmC_libC_road(2).trim.toLong

      val c_road = nmC_libC_road(3).trim.toLong

      val cities = Array.ofDim[Long](m.toInt, 2)

      for (i <- 0L until m) {
        cities(i.toInt) = stdin.readLine.split(" ").map(_.trim.toLong)
      }

      val result = roadsAndLibraries(n, c_lib, c_road, cities)

      printWriter.println(result)
    }

    printWriter.close()
  }
}
