import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Map, Queue, Set}

object Day21 {
  type Garden = ArrayBuffer[ArrayBuffer[String]]

// ...........
// .....###.#.
// .###.##..#.
// ..#.#...#..
// ....#.#....
// .##..S####.
// .##..#...#.
// .......##..
// .##.#.####.
// .##..##.##.
// ...........

  val directions = Seq(
    (1, 0), // East
    (-1, 0), // West
    (0, 1), // South
    (0, -1) // North
  )

  def parseInput(input: String): Garden =
    val lines = Source.fromFile(input).getLines()
    lines.map(line => line.split("").to(ArrayBuffer)).to(ArrayBuffer)

  def normalizeX(x: Int, width: Int): Int =
    var newX = x % width
    if (newX < 0) {
      newX = width + newX
    }

    newX
  
  def normalizeY(y: Int, height: Int): Int =
    var newY = y % height
    if (newY < 0) {
      newY = height + newY
    }

    newY

  extension (g: Garden)
    def at(x: Int, y: Int): String =
      if (x < 0 || y < 0 || y >= g.length || x >= g(0).length) {
        "#"
      } else {
        g(y)(x)
      }

    // the map repeats in all directions
    def at2(x: Int, y: Int, width: Int, height: Int): String =
      var newX = normalizeX(x, width)
      var newY = normalizeY(y, height)
      g.at(newX, newY)

    def show(occupied: Set[(Int, Int)], startX: Int = 0, startY: Int = 0) = {
      val width = g(0).length
      val height = g.length
      val lines = ArrayBuffer[String]()
      
      for (x <- startX until width) {
        val line = ArrayBuffer[String]()
        for (y <- startY until height) {
          val tile = g.at2(x, y, width, height)
          if (occupied.contains((x, y))) {
            line += "O"
          } else {
            line += tile
          }
        }

        lines += line.mkString("")
      }

      lines.mkString("\n")
    }

    // the frontier forms a rectangle
    // count the number of new wall hits on the edges of the rectangle
    def countWallHits(steps: Int, startX: Int, startY: Int): Long = {
      val side = g.length
      val wallCoords = Set[(Int, Int)]()

      for (y <- 0 until side) {
        for (x <- 0 until side) {
          if (g.at2(startX + x, startY + y, side, side) == "#") {
              wallCoords += ((startX + x, startY + y))
          }
        }
      }

      println(wallCoords)

      0
    }

    def showRepeating(occupied: Set[(Int, Int)]) = {
      val width = g(0).length
      val height = g.length
      var x, y = 0
      var minX, minY = Int.MaxValue
      var maxX, maxY = Int.MinValue
      
      for ((x, y) <- occupied) {
        if (x < minX) minX = x
        if (x > maxX) maxX = x
        if (y < minY) minY = y
        if (y > maxY) maxY = y
      }

      // minX -= 1
      // minY -= 1
      // maxX += 1
      // maxY += 1
      
      // println(s"minX: ${minX}, minY: ${minY}, maxX: ${maxX}, maxY: ${maxY}")

      val lines = ArrayBuffer[String]()

      for (y <- minY to maxY) {
        val line = ArrayBuffer[String]()
        for (x <- minX to maxX) {
          if (occupied.contains((x, y))) {
            line += "O"
          } else {
            val tile = g.at2(x, y, width, height)
            line += tile
          }
        }

        lines += line.mkString("")
      }

      lines.mkString("\n")
    }

    def startingPosition(): (Int, Int) = {
      val width = g(0).length
      val height = g.length
      var x, y = 0

      while (y < height) {
        x = 0
        while (x < width) {
          if (g.at(x, y) == "S") {
            return (x, y)
          }

          x += 1
        }

        y += 1
      }

      (-1, -1)
    }

    def walk(steps: Int) = {
      val width = g(0).length
      val height = g.length
      val visited = Set[(Int, Int)]()
      var prevSize = 0

      var occupied = Set[(Int, Int)]()
      var normalized = ArrayBuffer[(Int, Int)]()
      occupied += g.startingPosition()
      println(s"Starting pos: ${occupied}")

      for (i <- 1 to steps) {
        val newOccupied = Set[(Int, Int)]()
        normalized.clear()
        
        for ((x, y) <- occupied) {
          var foundDir = false

          for ((dx, dy) <- directions) {
            val newX = x + dx
            val newY = y + dy
            val normX = normalizeX(newX, width)
            val normY = normalizeY(newY, height)
            val tile = g.at(normX, normY)

            if (tile != "#") {
              foundDir = true
              newOccupied += ((newX, newY))
              normalized += ((normX, normY))
            }
          }

          if (!foundDir) {
            newOccupied += ((x, y))
          }
        }

        occupied = newOccupied
        // val key = normalized.mkString(",")

        // if (memo.contains(key)) {
        //   println(s"Found repeating pattern at step ${i}")
        // } else {
        //   memo += (key -> (occupied.size - prevSize))
        // }

        // println(s"steps: $i, count: ${occupied.size}, delta: ${occupied.size - prevSize}")
        // prevSize = occupied.size
        // println(g.showRepeating(occupied))
      }

      // println(g.showRepeating(occupied))
      // println()

      occupied
    }

    def walk3(steps: Int) = {
      val width = g(0).length
      val height = g.length

      val (startX, startY) = g.startingPosition()
      var frontier = Map[Int, (Int, Int)]() // row -> (minX, maxX)
      frontier(startY) = (startX, startX)

      for (i <- 1 to steps) {
        // println(s"frontier: $frontier")
        val newFrontier = Map[Int, (Int, Int)]()
        for ((y, (minX, maxX)) <- frontier) {
          // println(s"y: ${y}, step: ${i}")
          for (x <- Seq(minX, maxX)) {
            for ((dx, dy) <- directions) {
              val newX = x + dx
              val newY = y + dy
              val tile = g.at2(newX, newY, width, height)
              
              if (tile != "#") {
                val (minX, maxX) = newFrontier.getOrElse(newY, (Int.MaxValue, Int.MinValue))
                // println(s"newX: ${newX}, newY: ${newY}, minX: ${minX}, maxX: ${maxX}")
                newFrontier(newY) = (Math.min(minX, newX), Math.max(maxX, newX))
              }
            }
          }
        }

        // println(s"new frontier: $newFrontier")

        frontier = newFrontier
      }
      
      val occupied = Set.from(frontier.map(kv => {
        val (y, (minX, maxX)) = kv
        Seq((minX, y), (maxX, y))
      }).flatten)

      // println(g.showRepeating(occupied))
      // println()

      (frontier, occupied)
    }


  def part1(input: String) = {
    val garden = parseInput(input)
    val occupied = garden.walk(64)
    // println(garden.show(occupied))
    // println(occupied.mkString(", "))
    occupied.size
  }
  
  def part2(input: String) = {
    val garden = parseInput(input)
    val n = 10
    val (frontier, occ3) = garden.walk3(n)
    // val walls = garden.map(_.map(tile => if tile != "#" then 0 else 1).sum).sum
    val start = garden.startingPosition()
    val actual = garden.walk(n)
    // println(s"actual: ${actual}")
    println(s"start: ${start}")
    // println(s"yo: ${yo(garden, n)}")
    // println(occupied)
    // println(garden.showRepeating(occupied))

    var occupiedCells = 0L
    val width = garden(0).length
    val height = garden.length
    val occs = Set[(Int, Int)]()

    for ((y, (minX, maxX)) <- frontier) {
      for (x <- minX to maxX by 2) {
        val tile = garden.at2(x, y, width, height)
        if (tile != "#") {
          occupiedCells += 1
          occs += ((x, y))
        }
      }
    }

    var cells = garden.showRepeating(actual).split("\n").map(_.split("").to(ArrayBuffer))

    for ((x, y) <- actual.diff(occs)) {
      cells(y)(x) = "-"
    }

    for ((x, y) <- occs.diff(actual)) {
      cells(y)(x) = "+"
    }

    println("actual:")
    println(garden.showRepeating(actual))

    println("\nyo:")
    println(garden.showRepeating(occs))

    println("\ndiff:")
    println(cells.map(_.mkString("")).mkString("\n"))

    val diff = actual.diff(occs) ++ occs.diff(actual)
    println(diff)

    (occupiedCells, actual.size)
  }
}

// println(Day21.part1("input.txt"))
println(Day21.part2("sample.txt"))

