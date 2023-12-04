import scala.io.Source
import scala.collection.mutable._

object Day4 {
  case class Card(
      id: Int,
      winning: Set[Int],
      numbers: Seq[Int],
      var count: Int = 1
  )

  def parseCard(line: String): Card = line match
    case s"Card ${id}: ${winning} | ${numbers}" =>
      val winningNumbers = winning.trim.split("\\s+").map(_.trim.toInt)
      val numbersNumbers = numbers.trim.split("\\s+").map(_.trim.toInt)
      Card(id.trim.toInt, Set.from(winningNumbers), numbersNumbers)

  def parseInput(input: String): IndexedSeq[Card] =
    Source.fromFile(input).getLines.map(parseCard).toArray

  extension (c: Card)
    def countMatches: Int = c.numbers.count(c.winning.contains(_))

  def part1(input: String = "input.txt"): Int =
    parseInput(input).map(_.countMatches).map(1 << _ - 1).sum

  def part2(input: String = "input.txt"): Int =
    val cards = parseInput(input)

    for card <- cards do
      for i <- 1 to card.countMatches do
        cards(card.id + i - 1).count += card.count

    cards.map(_.count).sum
}

println(Day4.part1())
println(Day4.part2())
