package com.ciczan.algos

class Graph[A] {
  private val map = new scala.collection.mutable.HashMap[A, List[A]]()

  def vertices = map.keys

  def adjacent(node: A, other: A): Boolean = map(node).contains(other)
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
