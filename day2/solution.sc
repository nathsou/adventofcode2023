import scala.io.Source

object Day2 {
    type Rgb = (Int, Int, Int)
    case class Game(id: Int, rounds: Seq[Rgb])

    def keepDigits(line: String): Int = line.filter(_.isDigit).toInt

    def parseGame(line: String): Game =
        // Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        val parts = line.split(":")
        val id = parts(0).split(" ")(1).toInt
        val rounds = parts(1).split(";").map(_.trim).map { round =>
            val colors = round.split(",")
            var red, green, blue = 0

            colors.foreach(color =>
                if color.contains("red") then red = keepDigits(color)
                if color.contains("green") then green = keepDigits(color)
                if color.contains("blue") then blue = keepDigits(color)
            )

            (red, green, blue)
        }

        Game(id, rounds)

    def parseInput(): Iterator[Game] =
        Source.fromFile("input.txt").getLines.map(parseGame)

    def part1(input: String = "input.txt"): Int =
        parseInput().filter(_.rounds.forall((r, g, b) => 
            r <= 12 && g <= 13 && b <= 14
        )).map(_.id).sum
            
    def part2(input: String = "input.txt"): Int =
        parseInput().map(game =>
            val r = game.rounds.map((r, _, _) => r).max
            val g = game.rounds.map((_, g, _) => g).max
            val b = game.rounds.map((_, _, b) => b).max

            r * g * b
        ).sum
}

println(Day2.part1())
println(Day2.part2())
