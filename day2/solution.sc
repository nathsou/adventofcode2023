import scala.io.Source

object Day2 {
    case class Game(id: Int, rounds: List[(Int, Int, Int)])

    def keepInts(line: String): String =
        line.filter(c => c.isDigit)

    def parseGame(line: String): Game =
        // Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        val parts = line.split(":")
        val id = parts(0).split(" ")(1).toInt
        val rounds = parts(1).split(";").map(_.trim).map { round =>
            val colors = round.split(",")
            var red = 0
            var green = 0
            var blue = 0

            colors.foreach(color => {
                if color.contains("red") then red = keepInts(color).toInt
                if color.contains("green") then green = keepInts(color).toInt
                if color.contains("blue") then blue = keepInts(color).toInt
            })

            (red, green, blue)
        }.toList

        Game(id, rounds)

    def part1(input: String = "input.txt"): Int =
        val lines = Source.fromFile(input).getLines
        val games = lines.map(parseGame).toList

        def isPossible(game: Game, conf: (Int, Int, Int)): Boolean =
            game.rounds.forall((r, g, b) => r <= conf._1 && g <= conf._2 && b <= conf._3)

        val conf = (12, 13, 14)
        games.filter(isPossible(_, conf)).map(_.id).sum
            
    def part2(input: String = "input.txt"): Int =
        val lines = Source.fromFile(input).getLines
        val games = lines.map(parseGame).toList

        games.map(game => {
            val r = game.rounds.map((r, _, _) => r).max
            val g = game.rounds.map((_, g, _) => g).max
            val b = game.rounds.map((_, _, b) => b).max

            r * g * b
        }).sum
}

println(Day2.part1())
println(Day2.part2())
