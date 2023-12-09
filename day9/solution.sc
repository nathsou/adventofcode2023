import scala.io.Source

object Day9 {
  type Input = IndexedSeq[IndexedSeq[Long]]

  def parseInput(input: String, jokerAsWidlcard: Boolean = false): Input =
    val lines = Source.fromFile(input).getLines.toIndexedSeq
    lines.map(_.split(" ").map(_.toLong))

  def extrapolate(line: IndexedSeq[Long]): Long =
    val pairwiseDifferences = line.sliding(2).map { case Seq(a, b) => b - a }.toIndexedSeq
    if pairwiseDifferences.forall(_ == 0) then
      0
    else
      val delta = extrapolate(pairwiseDifferences)
      delta + pairwiseDifferences.last

  def extrapolateBackward(line: IndexedSeq[Long]): Long =
    val pairwiseDifferences = line.sliding(2).map { case Seq(a, b) => b - a }.toIndexedSeq
    if pairwiseDifferences.forall(_ == 0) then
      0
    else
      val delta = extrapolateBackward(pairwiseDifferences)
      pairwiseDifferences(0) - delta

  def part1(input: String = "input.txt"): Long =
    parseInput(input).map(line => line.last + extrapolate(line)).sum

  def part2(input: String = "input.txt"): Long =
    parseInput(input).map(line => line(0) - extrapolateBackward(line)).sum
}

println(Day9.part1())
println(Day9.part2())
