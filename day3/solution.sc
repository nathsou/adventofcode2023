import scala.io.Source
import scala.collection.mutable._

object Day3 {
    case class Num(n: Int, x: Int, y: Int, len: Int)

    def parseInput(input: String): IndexedSeq[Num] =
        val lines = Source.fromFile(input).getLines.toArray
        val nums = ArrayBuffer[Num]()
        var col = 0

        for y <- 0 until lines.length do
            var x = 0
            val line = lines(y)
            while x < line.length do
                val c = line(x)
                if c.isDigit then
                    val startX = x
                    val num = line.slice(x, line.length).takeWhile(_.isDigit)
                    nums += Num(num.toInt, startX, y, num.length)
                    x += num.length
                else
                    x += 1

        nums

    extension (lines: Array[String])
        def at(x: Int, y: Int): Char =
            if x < 0 || y < 0 || x >= lines(0).length || y >= lines.length then '.'
            else lines(y).charAt(x)

        def isSymbolAt(x: Int, y: Int): Boolean = at(x, y) match
            case '.' => false
            case c if c.isDigit => false
            case _ => true
        
    def part1(input: String = "input.txt"): Int =
        val lines = Source.fromFile(input).getLines.toArray

        def boundaryAround(x: Int, y: Int, len: Int): Seq[(Int, Int)] =
            val boundary = ArrayBuffer[(Int, Int)]()
            boundary.addOne((x - 1, y)) // left
            boundary.addOne((x + len, y)) // right

            for i <- -1 to len do
                boundary.addOne((x + i, y - 1)) // top
                boundary.addOne((x + i, y + 1)) // bottom

            boundary

        parseInput(input).filter(num =>
            boundaryAround(num.x, num.y, num.len).exists((x, y) => lines.isSymbolAt(x, y))
        ).map(_.n).sum
            
    def part2(input: String = "input.txt"): Int =
        val lines = Source.fromFile(input).getLines.toArray
        val nums = Map[(Int, Int), (Int, Int)]()
        for num <- parseInput(input) do
            val id = nums.size
            for i <- 0 until num.len do
                nums += ((num.x + i, num.y) -> (num.n, id))

        def isAdjacentTo(x1: Int, y1: Int, x2: Int, y2: Int): Boolean =
            if x1 == x2 && y1 == y2 then
                false
            else
                val dx = x1 - x2
                val dy = y1 - y2
                dx >= -1 && dx <= 1 && dy >= -1 && dy <= 1

        var sum = 0
        for y <- 0 until lines.length do
            for x <- 0 until lines(y).length do
                if lines.at(x, y) == '*' then
                    val adjacent = nums
                        .filter((pos, num) => isAdjacentTo(x, y, pos._1, pos._2))
                        .values
                        .groupBy(_._2)
                        .mapValues(_.head._1)
                        .values
                        .toArray

                    if adjacent.size == 2 then
                        sum += adjacent(0) * adjacent(1)

        sum
}

println(Day3.part1())
println(Day3.part2())
