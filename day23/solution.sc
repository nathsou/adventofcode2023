import scala.io.Source
import scala.collection.mutable.Stack

object Day23 {
  type Grid = Array[Array[String]]
  def parseInput(input: String): Grid =
    Source.fromFile(input).getLines().map(_.split("")).toArray

  val DIRS = Seq((0, 1), (1, 0), (0, -1), (-1, 0))

  extension (g: Grid)
    def at(x: Int, y: Int): String = 
      if (x < 0 || y < 0 || y >= g.length || x >= g(y).length) {
        return "#"
      }

      g(y)(x)

    def longestPath(startX: Int, startY: Int, endX: Int, endY: Int, isPart2: Boolean = false): Int =
      val stack = Stack[(Int, Int, Int, Set[(Int, Int)])]()
      stack.push((startX, startY, 0, Set((startX, startY))))
      var max = 0

      while (!stack.isEmpty) {
        val (x, y, dist, visited) = stack.pop()
        if (x == endX && y == endY) {
          if (dist > max) {
            max = dist
          }

          if (isPart2) {
            println(s"found path with dist $dist, current max is $max")
          }
        } else {
          DIRS.foreach((dx, dy) => {
            val nx = x + dx
            val ny = y + dy
            val tile = g.at(nx, ny)

            if (tile != "#" && !visited.contains((nx, ny))) {
              val canGo = isPart2 || (tile match {
                case "." => true
                case "v" => dx == 0 && dy == 1
                case "^" => dx == 0 && dy == -1
                case ">" => dx == 1 && dy == 0
                case "<" => dx == -1 && dy == 0
              })

              if (canGo) {
                stack.push((nx, ny, dist + 1, visited + ((nx, ny))))
              }
            }
          })
        }
      }

      max

  def part1(input: String = "input.txt") =
    val grid = parseInput(input)
    grid.longestPath(1, 0, grid(0).length - 2, grid.length - 1)

  def part2(input: String = "input.txt") =
    val grid = parseInput(input)
    grid.longestPath(1, 0, grid(0).length - 2, grid.length - 1, isPart2 = true)
}

println(Day23.part1())
println(Day23.part2())
