import scala.io.Source

object Day7 {
  enum Hand:
    case HighCard
    case OnePair
    case TwoPair
    case ThreeOfAKind
    case FullHouse
    case FourOfAKind
    case FiveOfAKind

  enum Card:
    case Any
    case N2
    case N3
    case N4
    case N5
    case N6
    case N7
    case N8
    case N9
    case T
    case J
    case Q
    case K
    case A
  
  case class Bid(hand: List[Card], bid: Int)
    
  def parseInput(input: String, jokerAsWidlcard: Boolean = false): List[Bid] =
    Source.fromFile(input).getLines.map { line => line match
      case s"$hand $bid" =>
        val h = hand.trim.split("").map(c => c match
          case "2" => Card.N2
          case "3" => Card.N3
          case "4" => Card.N4
          case "5" => Card.N5
          case "6" => Card.N6
          case "7" => Card.N7
          case "8" => Card.N8
          case "9" => Card.N9
          case "T" => Card.T
          case "J" if !jokerAsWidlcard => Card.J
          case "J" if jokerAsWidlcard => Card.Any
          case "Q" => Card.Q
          case "K" => Card.K
          case "A" => Card.A
        ).toList

        Bid(h, bid.trim.toInt)
      }.toList

  def combinationsWithRepetitions[T](l: List[T], n: Int): List[List[T]] = n match
    case 0 => List(List())
    case _ => for (el <- l; sl <- combinationsWithRepetitions(l.dropWhile(_ != el), n - 1))
              yield el :: sl

  extension  (hand: List[Card])
    def ty: Hand =
      val groups = hand.groupBy(identity).mapValues(_.length).values.toArray
      groups.max match
        case 5 => Hand.FiveOfAKind
        case 4 => Hand.FourOfAKind
        case 3 if groups.count(_ == 2) == 1 => Hand.FullHouse
        case 3 => Hand.ThreeOfAKind
        case _ if groups.count(_ == 2) == 2 => Hand.TwoPair
        case 2 => Hand.OnePair
        case _ => Hand.HighCard

    def substitute(replaceWith: List[Card], acc: List[Card] = List()): List[Card] =
      hand match
        case Nil => acc
        case c :: cs => c match
          case Card.Any => cs.substitute(replaceWith, replaceWith.head :: acc)
          case _ => cs.substitute(replaceWith, c :: acc)

    def typeWithWildcards: Hand =
      hand.count(_ == Card.Any) match
        case 0 => hand.ty
        case count => combinationsWithRepetitions(
            Card.values.filter(_ != Card.Any).toList,
            count
          ).map(hand.substitute(_)).map(_.ty).maxBy(_.ordinal)
      
    def cmp(hand2: List[Card]): Boolean =
      val h1ty = hand.typeWithWildcards.ordinal
      val h2ty = hand2.typeWithWildcards.ordinal

      if h1ty > h2ty then true
      else if h1ty < h2ty then false
      else 
        var i = 0
        while i < hand.length do
          if hand(i).ordinal > hand2(i).ordinal then return true
          else if hand(i).ordinal < hand2(i).ordinal then return false
          i += 1

        false
        
  def part1(input: String = "input.txt"): Int =
    val bids = parseInput(input).sortWith((a, b) => !a.hand.cmp(b.hand))
    bids.zipWithIndex.map((b, i) => b.bid * (i + 1)).sum

  def part2(input: String = "input.txt"): Int =
    val bids = parseInput(input, jokerAsWidlcard = true).sortWith((a, b) => !a.hand.cmp(b.hand))
    bids.zipWithIndex.map((b, i) => b.bid * (i + 1)).sum
}

println(Day7.part1())
println(Day7.part2())
