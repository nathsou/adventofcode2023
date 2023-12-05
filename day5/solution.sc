import scala.io.Source
import scala.collection.mutable._
import sourcecode.Text.generate

object Day5 {
  case class Range(dest: BigInt, src: BigInt, len: BigInt)
  case class Maps(
    seeds: Seq[BigInt],
    mappings: Seq[(String, Seq[Range])],
  )

  def parseInput(input: String): Maps =
    val inp = Source.fromFile(input).mkString.split("\n\n").map(_.trim.replaceAll("\n", " "))
    val seeds = ArrayBuffer[BigInt]()
    val mappings = ArrayBuffer[(String, Seq[Range])]()

    for line <- inp do
      line match
        case s"$name:$rhs" => {
          if name == "seeds" then
            seeds ++= rhs.trim.split(" ").map(_.trim).map(BigInt.apply(_))
          else
            val ranges = rhs.trim.split(" ").map(_.trim).map(BigInt.apply(_)).grouped(3)
            val rngs = ArrayBuffer[Range]() 
            for r <- ranges do
              rngs += Range(r(0), r(1), r(2))
            mappings += ((name, rngs.toArray))
        }

    Maps(seeds.toArray, mappings.toArray)

  extension (rngs: Seq[Range])
    def map(n: BigInt): BigInt =
      rngs.find(r => n >= r.src && n < r.src + r.len) match
        case Some(r) => r.dest + n - r.src
        case None => n

  def part1(input: String = "input.txt"): BigInt =
    val maps = parseInput(input)
    val locations = ArrayBuffer[BigInt]()
    
    for seed <- maps.seeds do
      var n = seed
      for (name, ranges) <- maps.mappings do
        n = ranges.map(n)

      locations += n
      
    locations.min
    

  def part2(input: String = "input.txt"): BigInt =
    val maps = parseInput(input)
    val seedRanges = maps.seeds.grouped(2).map(g => (g(0), g(1))).toArray
    var minLoc = BigInt("100000000000")
    
    for (start, len) <- seedRanges do
      println(s"start: $start, len: $len")
      for seed <- start until start + len do
        var n = seed
        for (name, ranges) <- maps.mappings do
          n = ranges.map(n)

        if n < minLoc then
          minLoc = n
      
    minLoc
}

println(Day5.part1())
println(Day5.part2())
