import scala.io.Source

object Day8 {
  enum Insruction:
    case Left
    case Right

  case class Input(
      instructions: Seq[Insruction],
      nodes: Map[String, (String, String)]
  )

  def parseInput(input: String, jokerAsWidlcard: Boolean = false): Input =
    val lines = Source.fromFile(input).getLines.toList
    val instructions = lines(0).trim
      .split("")
      .map(c =>
        c match
          case "L" => Insruction.Left
          case "R" => Insruction.Right
      )
      .toArray

    val map = Map.from(lines.drop(2).map { line =>
      line match
        case s"$lhs = ($l, $r)" => lhs -> (l, r)
    })

    Input(instructions, map)

  extension (dir: (String, String))
    def followInstruction(instruction: Insruction): String = instruction match
      case Insruction.Left  => dir._1
      case Insruction.Right => dir._2

  def part1(input: String = "input.txt"): Int =
    val inp = parseInput(input)
    var start = "AAA"
    var index = 0

    while start != "ZZZ" do
      val instruction = inp.instructions(index % inp.instructions.length)
      val dest = inp.nodes(start).followInstruction(instruction)
      index += 1
      start = dest

    index

  def cycle[A](it: Iterable[A]): Iterator[A] = Iterator.continually(it).flatten

  def stepsToReachZ(inp: Input, start: String): Option[Long] =
    var steps = 1
    var node = start

    while !node.endsWith("Z") do
      val inst = inp.instructions(steps % inp.instructions.length)
      inp.nodes(node) match
        case (l, r) if l == r && l == node => return None
        case _ =>
          node = inp.nodes(node).followInstruction(inst)
          steps += 1

    Some(steps)

  def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)
  def llcm(a: Long, b: Long): Long = (a * b) / gcd(a, b)
  def llcm(ns: Iterable[Long]): Long = ns.reduce((a, b) => llcm(a, b))

  def part2(input: String = "input.txt"): Long =
    val inp = parseInput(input)
    val stepsToReachZMap =
      inp.nodes.mapValues((node, _) => stepsToReachZ(inp, node))
    val startingNodes = inp.nodes.filterKeys(_.endsWith("A")).keys
    llcm(startingNodes.map(stepsToReachZMap(_)).collect({ case Some(v) => v }))
}

println(Day8.part1())
println(Day8.part2())
