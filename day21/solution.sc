import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Map, Queue, Set}

object Day21 {
  type Garden = ArrayBuffer[ArrayBuffer[String]]

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
      // println(s"Starting pos: ${occupied}")

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
      }

      occupied
    }
 
    def freeSpots(): (Long, Long) = {
      val width = g(0).length
      val height = g.length
      var even = 0L
      var odd = 0L

      for (y <- 0 until height) {
        for (x <- 0 until width) {
          val tile = g.at(x, y)
          if (tile != "#") {
            if ((x + y) % 2 == 0) {
              even += 1
            } else {
              odd += 1
            }
          }
        }
      }

      (even, odd)
    }

    def distances(start: (Int, Int), steps: Int) = {
      var prevDists: ArrayBuffer[(Int, Int)] = ArrayBuffer(start)
      val visited = Map[(Int, Int), Int]() // pos -> steps of last visit
      val width = g(0).length
      val height = g.length
      var total = ArrayBuffer(1, 0)

      visited += (start -> 0)

      for (step <- 1 to steps) {
        // all points at distance step
        val next = ArrayBuffer[(Int, Int)]()

        for ((x, y) <- prevDists) {
          for ((dx, dy) <- directions) {
            val newX = x + dx
            val newY = y + dy
            val normX = normalizeX(newX, width)
            val normY = normalizeY(newY, height)
            val tile = g.at(normX, normY)
            val pos = (newX, newY)

            if (tile != "#") {
              if (!visited.contains(pos)) {
                visited += (pos -> step)
                next += pos
              } else {
                visited(pos) = step
              }
            }
          }
        }

        total(step % 2) += next.size
        prevDists = next

        if (step % 20 == 0) {
          println(s"step: ${step}, total: ${total.max}, ${(step.toFloat / steps.toFloat * 100).formatted("%.2f")}%")
          // cleanup visited if last visit was more than 2 steps ago
          for ((pos, lastStep) <- visited) {
            if (step - lastStep > 1) {
              visited -= pos
            }
          }
        }
      }

      total.max
    }

  def part1(input: String) = {
    val garden = parseInput(input)
    val occupied = garden.walk(64)
    occupied.size
  }
  
  def part2(input: String) = {
    val garden = parseInput(input)
    val side = garden.length
    val n = 5000
    val start = garden.startingPosition()
    val dists = garden.distances(start, n)
  }
}

// println(Day21.part1("input.txt"))
println(Day21.part2("sample.txt"))
