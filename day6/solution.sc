import scala.io.Source

object Day6 {
  case class Race(time: Long, distance: Long)

  def parseInput(input: String): Seq[Race] =
    input match
      case s"Time:$times Distance:$distances" =>
        val ts = times.trim.split("\\s+").map(_.trim.toLong)
        val ds = distances.trim.split("\\s+").map(_.trim.toLong)
        ts.zip(ds).map((t, d) => Race(t, d))

  extension (r: Race)
    def wins: Long = (0L until r.time).count(v => v * (r.time - v) > r.distance)

  def part1(input: String = "input.txt"): Long =
    val races = Source.fromFile(input).mkString.split("\n").mkString(" ")
    parseInput(races).map(_.wins).product

  def part2(input: String = "input.txt"): Long =
    val races = Source.fromFile(input).mkString.split("\n").map(_.replaceAll(" ", "")).mkString(" ")
    parseInput(races)(0).wins
}

println(Day6.part1())
println(Day6.part2())
