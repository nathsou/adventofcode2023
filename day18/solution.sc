import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Day18 {
  enum Dir:
    case R, L, U, D

    def delta(): (Long, Long) = this match {
      case Dir.R => (1, 0)
      case Dir.L => (-1, 0)
      case Dir.U => (0, 1)
      case Dir.D => (0, -1)
    }

  def parseDir(s: String): Dir = s match {
    case "R" => Dir.R
    case "L" => Dir.L
    case "U" => Dir.U
    case "D" => Dir.D
  }

  case class DigLine(dir: Dir, len: Long, color: String)
  case class DirLen(dir: Dir, len: Long)

  def parseInput(input: String): Seq[DigLine] =
    Source.fromFile(input).getLines.map(line => line match {
      case s"$dir $len (#$color)" => DigLine(parseDir(dir), len.toLong, color)
    }).toSeq

  // https://www.wikiwand.com/en/Shoelace_formula
  // trapezoid formula
  def interiorArea(points: ArrayBuffer[(Long, Long)]): Long =
    (points.sliding(2).map(ps => {
      val (x1, y1) = ps(0)
      val (x2, y2) = ps(1)
      (y1 + y2) * (x1 - x2)
    }).sum / 2).abs

  def totalArea(lines: Seq[DirLen]): Long =
    val points = ArrayBuffer[(Long, Long)]()
    var perimeter, x, y = 0L

    for line <- lines do
      val (dx, dy) = line.dir.delta()
      x += dx * line.len
      y += dy * line.len
      perimeter += line.len
      points += ((x, y))

    // half of the perimeter isn't accounted for, i guess
    interiorArea(points) + perimeter / 2 + 1

  // for the first solution, I used a flood fill
  def part1(input: String = "input.txt") =
    totalArea(parseInput(input).map(line => DirLen(line.dir, line.len)))

  def part2(input: String = "input.txt") =
    val DIRS = Array(Dir.R, Dir.D, Dir.L, Dir.U)
    totalArea(parseInput(input).map(line => {
      val dir = DIRS(line.color.last.toInt - '0'.toInt)
      val len = java.lang.Long.parseLong(line.color.dropRight(1), 16)
      DirLen(dir, len)
    }))
}

println(Day18.part1())
println(Day18.part2())
