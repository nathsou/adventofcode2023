import scala.io.Source
import scala.collection.mutable._

object Day4 {
  case class Card(
      id: Int,
      winningNumbers: Set[Int],
      numbers: Seq[Int],
      var count: Int = 1
  )

  def parseCard(line: String): Card = line match
    case s"Card ${id}: ${winning} | ${numbers}" =>
      val winningNumbers =
        winning.trim().split(' ').filter(n => n.trim().length > 0).map(_.toInt)
      val numbersNumbers =
        numbers.trim().split(' ').filter(n => n.trim().length > 0).map(_.toInt)
      Card(id.trim.toInt, Set.from(winningNumbers), numbersNumbers)

  def parseInput(input: String): IndexedSeq[Card] =
    Source.fromFile(input).getLines.map(parseCard).toArray

  extension (c: Card)
    def countMatches: Int = c.numbers.count(c.winningNumbers.contains(_))

  def part1(input: String = "input.txt"): Int =
    parseInput(input).map(_.countMatches).map(n => Math.pow(2, n - 1).toInt).sum

  def part2(input: String = "input.txt"): Int =
    val cards = parseInput(input)

    for card <- cards do
      val wins = card.countMatches
      for i <- 1 to wins do
        cards.find(_.id == card.id + i).map(c => c.count = c.count + card.count)

    cards.map(_.count).sum
}

println(Day4.part1())
println(Day4.part2())
