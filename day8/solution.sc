import scala.io.Source
import scala.collection.mutable._

object Day8 {
  enum Insruction:
    case Left
    case Right

  case class Input(insructions: Seq[Insruction], map: Map[String, (String, String)])
  
  def parseInput(input: String, jokerAsWidlcard: Boolean = false): Input =
    val lines = Source.fromFile(input).getLines.toList
    val instructions = lines(0).trim.split("").map(c => c match
      case "L" => Insruction.Left
      case "R" => Insruction.Right
    ).toArray

    val map = Map.from(lines.drop(2).map { line => line match
      case s"$lhs = ($l, $r)" =>
        lhs -> (l, r)
    })

    Input(instructions, map)

  extension (dir: (String, String))
    def followInstruction(instruction: Insruction): String = instruction match
      case Insruction.Left => dir._1
      case Insruction.Right => dir._2
        
  def part1(input: String = "input.txt"): Int =
    val inp = parseInput(input)

    var start = "AAA"
    var index = 0

    while start != "ZZZ" do
      val instruction = inp.insructions(index % inp.insructions.length)
      val dest = inp.map(start).followInstruction(instruction)
      index += 1
      start = dest

    index

  def getDest(inp: Input, start: String): (String, Set[Int]) =
    var node = start
    var index = 0
    val nodesEndingWithZ = Set[Int]()

    for inst <- inp.insructions do
      node = inp.map(node).followInstruction(inst)
      index += 1
      if node.endsWith("Z") then
        nodesEndingWithZ += index

    (node, nodesEndingWithZ)

  def gcd(a: Long, b: Long): Long = b match
    case 0 => a
    case n => gcd(b, a % b)

  def llcm(a: Long, b: Long): Long = (a * b) / gcd(a, b)
  def llcm(ns: Seq[Long]): Long = ns.reduce((a, b) => llcm(a, b))

  def part2(input: String = "input.txt"): Long =
    val inp = parseInput(input)

    var startingNodes = ArrayBuffer.from(inp.map.filterKeys(_.endsWith("A")).keys)

    // destination after all the instructions
    val memo = Map[String, (String, Set[Int])]()

    inp.map.keys.foreach { node =>
      memo(node) = getDest(inp, node)
    }

    val minSteps = Map[String, Int]()

    inp.map.keys.foreach { node =>
      var steps = 0
      var n = node

      while !n.endsWith("Z") do
        val (destNode, zs) = memo(n)
        if destNode == n then
          steps = Int.MaxValue
          n = "Z"
        else if zs.nonEmpty then
          steps += zs.min
          n = "Z"
        else
          steps += inp.insructions.length
          n = destNode
        
      minSteps(node) = steps
    }

    llcm(startingNodes.map(minSteps(_).toLong))
}

println(Day8.part1())
println(Day8.part2())
