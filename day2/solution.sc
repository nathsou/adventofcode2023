import scala.io.Source

object Day2 {
    type Rgb = (Int, Int, Int)
    case class Game(id: Int, rounds: Seq[Rgb])

    def parseGame(line: String): Game =
        line match
            case s"Game $id: $rhs" =>
                val rounds = rhs.split(";").map { round =>
                    var red, green, blue = 0

                    round.split(",").foreach { color => color match
                        case s"$n red" => red = n.trim.toInt
                        case s"$n green" => green = n.trim.toInt
                        case s"$n blue" => blue = n.trim.toInt
                    }

                    (red, green, blue)
                }

                Game(id.toInt, rounds)

    def parseInput(input: String): Iterator[Game] =
        Source.fromFile(input).getLines.map(parseGame)

    def part1(input: String = "input.txt"): Int =
        parseInput(input).filter(_.rounds.forall((r, g, b) => 
            r <= 12 && g <= 13 && b <= 14
        )).map(_.id).sum
            
    def part2(input: String = "input.txt"): Int =
        parseInput(input).map(game =>
            val r = game.rounds.map((r, _, _) => r).max
            val g = game.rounds.map((_, g, _) => g).max
            val b = game.rounds.map((_, _, b) => b).max

            r * g * b
        ).sum
}

println(Day2.part1())
println(Day2.part2())
