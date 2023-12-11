import scala.io.Source
import scala.collection.mutable._

object Day11 {
  type Universe = Array[Array[Boolean]]
  case class Expanded(rows: Set[Int], cols: Set[Int])

  def parseInput(input: String): Universe =
    Source
      .fromFile(input)
      .getLines
      .map(line => line.split("").map(_ == "#").toArray)
      .toArray

  extension (u: Universe)
    def galaxies() =
      for
        i <- 0 until u.length
        j <- 0 until u(i).length if u(i)(j)
      yield (i, j)

    def emptyRowsAndCols(): Expanded =
      var emptyRows = Set[Int]()
      var emptyCols = Set[Int]()

      for (i <- 0 until u.length) {
        if (!u(i).contains(true)) {
          emptyRows += i
        }
      }

      for (j <- 0 until u(0).length) {
        if (!u.map(_(j)).contains(true)) {
          emptyCols += j
        }
      }

      Expanded(emptyRows, emptyCols)

    def sumOfDists(expansionFactor: Long): Long =
      val expanded = u.emptyRowsAndCols()
      exclusivePairs(u.galaxies().toArray)
        .map((a, b) => expanded.distance(a, b, expansionFactor))
        .sum

  extension (e: Expanded)
    def distance(
        a: (Int, Int),
        b: (Int, Int),
        expansionFactor: Long
    ): Long =
      val xs = Range(a._1.min(b._1), a._1.max(b._1))
      val ys = Range(a._2.min(b._2), a._2.max(b._2))
      val expandedRows = e.rows.count(xs.contains(_))
      val expandedCols = e.cols.count(ys.contains(_))

      xs.length + ys.length + (expandedRows + expandedCols) * (expansionFactor - 1)

  def exclusivePairs[T](xs: IndexedSeq[T]) =
    for {
      i <- 0 until xs.length
      j <- i + 1 until xs.length
    } yield (xs(i), xs(j))

  def part1(input: String = "input.txt") = parseInput(input).sumOfDists(2)

  def part2(input: String = "input.txt") =
    parseInput(input).sumOfDists(1000000)
}

println(Day11.part1())
println(Day11.part2())
