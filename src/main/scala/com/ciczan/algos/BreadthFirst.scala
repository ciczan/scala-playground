package com.ciczan.algos

object BreadthFirst {

  def searchDepth[A](node: A, graph: Graph[A], f: A => Boolean): Option[(Int, A)] =  {

    def go(depth: Int, nodes: Seq[A], visited: Set[A]): Option[(Int, A)] = {
      val fNodes = nodes.diff(visited.toSeq)
      if (fNodes.isEmpty) None
      else fNodes.find(f) match {
          case Some(i) => Option(depth, i)
          case None => go(depth + 1, fNodes.flatMap(graph.neighbors), visited ++ fNodes)
      }
    }

    if (f(node)) Option(0, node)
    else go(1, graph.neighbors(node), Set(node))
  }

  def search[A](start: A, graph: Graph[A], f: A => Boolean): Option[A] =  {

    val myQueue = scala.collection.mutable.Queue[A]()
    myQueue ++= graph.neighbors(start)

    var visited = Set[A]()

    var opt: Option[A] = None
    while (myQueue.nonEmpty && opt.isEmpty) {
      val node = myQueue.dequeue()
      if (!visited.contains(node)) {
        if (f(node)) opt = Option(node)
        else {
          visited += node
          myQueue ++= graph.neighbors(node)
        }
      }
    }
    opt
  }

}
