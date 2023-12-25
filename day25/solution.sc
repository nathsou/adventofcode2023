import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Map, Set, Stack}

object Day25 {
  class Graph[L]() {
    val nodes = Set[L]()
    val edges = Map[L, Set[L]]()

    def addNode(node: L) = nodes += node

    def addEdge(from: L, to: L) =
      if (!edges.contains(from)) {
        edges += (from -> Set[L]())
      }

      edges(from) += to

    def removeEdge(from: L, to: L) =
      if (edges.contains(from)) {
        edges(from) -= to
      }
      
      if (edges.contains(to)) {
        edges(to) -= from
      }

    def adjacentNodes(node: L): Set[L] =
      val adj = Set.from(edges.filter((k, v) => v.contains(node)).keys)

      if (edges.contains(node)) {
        adj ++= edges(node)
      }

      adj

    def toDotFile(file: String) =
      val dot = StringBuilder()
      dot ++= "graph {\n"

      for ((from, tos) <- edges) {
        for (to <- tos) {
          dot ++= s"  ${from} -- ${to} [id=\"$from:$to\"]\n"
        }
      }

      dot ++= "}"
      val writer = java.io.PrintWriter(file)
      writer.write(dot.toString())
      writer.close()

    def connectedComponent(start: L): Set[L] =
      val visited = Set[L]()
      val stack = Stack[L]()
      stack.push(start)

      while (!stack.isEmpty) {
        val node = stack.pop()
        if (!visited.contains(node)) {
          visited += node
          for (to <- adjacentNodes(node)) {
            stack.push(to)
          }
        }
      }

      visited
  }

  def parseInput(input: String) =
    val graph = Graph[String]()
    Source.fromFile(input).getLines().foreach(line => line match {
      case s"${from}: ${tos}" => {
        graph.addNode(from)
        tos.split(" ").foreach(to => {
          graph.addNode(to)
          graph.addEdge(from, to)
        })
      }
    })

    graph

  
  def part1(input: String = "input.txt") =
    val graph = parseInput(input)
    graph.toDotFile("graph.dot")
    // run dot graph.dot -Tsvg -O
    // and open it in a browser
    // using the dev tools, select the three edges crossing the two components
    // and remove them

    graph.removeEdge("fmr", "zhg")
    graph.removeEdge("krf", "crg")
    graph.removeEdge("rgv", "jct")

    val comp1 = graph.connectedComponent("pzr")
    val comp2 = graph.connectedComponent("rkz")

    comp1.size * comp2.size
}

println(Day25.part1())
