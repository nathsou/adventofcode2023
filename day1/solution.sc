import scala.io.Source
import scala.collection.mutable.ArrayBuffer

object Day1 {
    def part1(input: String = "input.txt"): Int =
        val lines = Source.fromFile(input).getLines().toList
        lines.map(line =>
            val digits = line.filter(_.isDigit).toArray
            if digits.isEmpty then 0
            else (digits(0).toString() + digits.last.toString()).toInt
        ).sum
            
    def part2(input: String = "input.txt"): Int =
        extension (str: String)
            def at(index: Int): Char | Null =
                if index >= str.length then null
                else str(index)

        val lines = Source.fromFile("input.txt").getLines().toArray

        // manual implementation of a trie for the digits
        def getDigit(str: String, index: Int): Option[(Int, Int)] =
            str.at(index) match {
                case 'o' => str.at(index + 1) match {
                    case 'n' => str.at(index + 2) match {
                        case 'e' => Some((1, 3))
                        case _ => None
                    }
                    case _ => None
                }
                case 't' => str.at(index + 1) match {
                    case 'w' => str.at(index + 2) match {
                        case 'o' => Some((2, 3))
                        case _ => None
                    }
                    case 'h' => str.at(index + 2) match {
                        case 'r' => str.at(index + 3) match {
                            case 'e' => str.at(index + 4) match {
                                case 'e' => Some((3, 5))
                                case _ => None
                            }
                            case _ => None
                        }
                        case _ => None
                    }
                    case _ => None
                }
                case 'f' => str.at(index + 1) match {
                    case 'o' => str.at(index + 2) match {
                        case 'u' => str.at(index + 3) match {
                            case 'r' => Some((4, 4))
                            case _ => None
                        }
                        case _ => None
                    }
                    case 'i' => str.at(index + 2) match {
                        case 'v' => str.at(index + 3) match {
                            case 'e' => Some((5, 4))
                            case _ => None
                        }
                        case _ => None
                    }
                    case _ => None
                }
                case 's' => str.at(index + 1) match {
                    case 'i' => str.at(index + 2) match {
                        case 'x' => Some((6, 3))
                        case _ => None
                    }
                    case 'e' => str.at(index + 2) match {
                        case 'v' => str.at(index + 3) match {
                            case 'e' => str.at(index + 4) match {
                                case 'n' => Some((7, 5))
                                case _ => None
                            }
                            case _ => None
                        }
                        case _ => None
                    }
                    case _ => None
                }
                case 'e' => str.at(index + 1) match {
                    case 'i' => str.at(index + 2) match {
                        case 'g' => str.at(index + 3) match {
                            case 'h' => str.at(index + 4) match {
                                case 't' => Some((8, 5))
                                case _ => None
                            }
                            case _ => None
                        }
                        case _ => None
                    }
                    case _ => None
                }
                case 'n' => str.at(index + 1) match {
                    case 'i' => str.at(index + 2) match {
                        case 'n' => str.at(index + 3) match {
                            case 'e' => Some((9, 4))
                            case _ => None
                        }
                        case _ => None
                    }
                    case _ => None
                }
                case _ => None
            }

        def getDigits(line: String): ArrayBuffer[Int] =
            val digits = ArrayBuffer[Int]()
            var index = 0

            while index < line.length do
                if line(index).isDigit then
                    digits += line.at(index).toString().toInt
                    index += 1
                else getDigit(line, index) match
                    case Some((digit, len)) =>
                        digits += digit
                        index += len - 1
                    case None => index += 1

            digits

        lines.map(line => {
            val digits = getDigits(line)
            (digits(0).toString() + digits.last.toString()).toInt
        }).sum
}

println(Day1.part1())
println(Day1.part2("sample.txt"))
