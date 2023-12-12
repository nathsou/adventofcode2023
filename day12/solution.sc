import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import os.read.lines

object Day12 {
  enum State:
    case Unknown
    case Empty
    case Filled

  case class Row(cells: IndexedSeq[State], criteria: IndexedSeq[Int])
  type Input = IndexedSeq[Row]

  def parseInput(input: String): Input =
    val lines = Source.fromFile(input).getLines
    lines.map { row => row match {
      case s"$cells $criteria" => {
        val cs = cells.map { cell => cell match {
          case '.' => State.Empty
          case '#' => State.Filled
          case '?' => State.Unknown
        }}

        val crit = criteria.split(",").map(_.toInt)
        Row(cs, crit)
      }}
    }.toIndexedSeq

  extension (row: Row)
    def substitute(unknownIndices: IndexedSeq[Int], states: ArrayBuffer[State]): Row =
      val copy = row.cells.toBuffer
      
      unknownIndices.zip(states).foreach { (i, state) =>
        copy(i) = state
      }

      Row(copy.toIndexedSeq, row.criteria)

    def substitute2(filledIndices: IndexedSeq[Int]): Row =
      val copy = row.cells.toBuffer
      filledIndices.foreach { i => copy(i) = State.Filled }

      for i <- 0 until copy.length do
        if copy(i) == State.Unknown then copy(i) = State.Empty

      Row(copy.toIndexedSeq, row.criteria)

    def str(): String = {
      row.cells.map { cell => cell match {
        case State.Empty => '.'
        case State.Filled => '#'
        case State.Unknown => '?'
      }}.mkString("")
    }
    
    def isSatisfied: Boolean =
      val groups = row.str().split("\\.+").filter(_.length > 0)

      groups.length == row.criteria.length && groups.zip(row.criteria).forall { (group, crit) =>
        group.length == crit
      }

    def countArrangements(): Long =
      if row.isSatisfied then return 1
      val unknownIndices = row.cells.zipWithIndex.filter(_._1 == State.Unknown).map(_._2)
      val missingFilled = row.criteria.sum - row.cells.count(_ == State.Filled)
      var states = ArrayBuffer.fill(unknownIndices.length)(State.Empty)

      for i <- 0 until missingFilled do
        states(i) = State.Filled

      val count = states.permutations.count { states =>
        row.substitute(unknownIndices, states).isSatisfied
      }

      count

    def fold(): Row =
      var foldedRow = ArrayBuffer[State]()
      var foldedCriteria = ArrayBuffer[Int]()

      for _ <- 0 until 5 do
        foldedRow ++= row.cells
        foldedRow += State.Unknown
        foldedCriteria ++= row.criteria

      Row(foldedRow.toIndexedSeq, foldedCriteria.toIndexedSeq)

  def part1(input: String = "input.txt"): Long =
    val inp = parseInput(input)
    inp.map(_.countArrangements()).sum
}

println(Day12.part1())
