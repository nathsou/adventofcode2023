import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Day1 {
  def part1(): Int =
    val lines = Source.fromFile("input.txt").getLines().toList
    lines.map(line => {
        val digits = line.filter(_.isDigit).toList
        val concat = digits(0).toString() + digits(digits.length - 1).toString()
        concat.toInt
    }).sum

  def part2(): Int =
        val lines = Source.fromFile("input.txt").getLines().toList
        val digits = Map(
            "one" -> 1,
            "two" -> 2,
            "three" -> 3,
            "four" -> 4,
            "five" -> 5,
            "six" -> 6,
            "seven" -> 7,
            "eight" -> 8,
            "nine" -> 9,
        )

        def getDigits(line: String): ArrayBuffer[Int] =
            var res = ArrayBuffer[Int]()
            
            Range(0, line.length).foreach(i => {
                if (line(i).isDigit) {
                    res.addOne(line(i).toString().toInt)
                } else {
                    for ((key, value) <- digits.iterator) {
                        if (line.slice(i, line.length()).startsWith(key)) {
                            res.addOne(value)
                        }
                    }
                }
            })

            res
                

        lines.map(line => {
            val digits = getDigits(line)
            (digits(0).toString() + digits(digits.length - 1).toString()).toInt
        }).sum
}

println(Day1.part1())
println(Day1.part2())
