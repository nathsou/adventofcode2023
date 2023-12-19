import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Stack
import scala.collection.mutable.Set

object Day19 {
  enum Condition:
    case Lss, Gtr

  case class Rule(prop: String, cond: Condition, value: Int, target: String)
  case class Cond(prop: String, cond: Condition, value: Int)
  case class Workflow(name: String, rules: Seq[Rule], fallback: String)
  case class Part(x: Int, m: Int, a: Int, s: Int)
  case class Interval(var min: Int, var max: Int)
  case class Intervals(x: Interval, m: Interval, a: Interval, s: Interval)

  def parseInput(input: String): (Map[String, Workflow], Seq[Part]) =
    val lines = Source.fromFile(input).mkString.split("\n\n")
    val workflows = lines(0).split("\n").map(parseWorkflow)
    val parts = lines(1).split("\n").map(parsePart)
    (workflows.map(w => w.name -> w).toMap, parts)

  def parseWorkflow(line: String) = line match {
    case s"$name{$rules}" =>
      val rs = rules.split(",").toIndexedSeq
      Workflow(name, rs.dropRight(1).map(parseRule), rs.last)
  }

  def parseRule(rule: String) = rule match {
    case s"$prop<$value:$target" =>
      Rule(prop, Condition.Lss, value.toInt, target)
    case s"$prop>$value:$target" =>
      Rule(prop, Condition.Gtr, value.toInt, target)
  }

  def parsePart(part: String): Part = part match {
    case s"{x=$x,m=$m,a=$a,s=$s}" =>
      Part(x.toInt, m.toInt, a.toInt, s.toInt)
  }

  extension (p: Part)
    def get(prop: String) = prop match
      case "x" => p.x
      case "m" => p.m
      case "a" => p.a
      case "s" => p.s

    def matches(rule: Rule) = rule.cond match
      case Condition.Lss => p.get(rule.prop) < rule.value
      case Condition.Gtr => p.get(rule.prop) > rule.value

    def run(w: Workflow): String =
      w.rules.find(matches) match
        case Some(r) => r.target
        case None => w.fallback
    
    def finalState(workflows: Map[String, Workflow]): String =
      var state = "in"

      while state != "R" && state != "A" do
        val w = workflows(state)
        state = run(w)

      state

    
  def part1(input: String = "input.txt") =
    val (workflows, parts) = parseInput(input)
    val finalStates = parts.map(_.finalState(workflows))
    parts.filter(_.finalState(workflows) == "A").map(p => p.x + p.m + p.a + p.s).sum

  extension (interval: Interval)
    def len(): Long = interval.max.toLong - interval.min.toLong + 1L

  extension (intervals: Intervals)
    def setMin(prop: String, value: Int) = prop match
      case "x" => intervals.x.min = value
      case "m" => intervals.m.min = value
      case "a" => intervals.a.min = value
      case "s" => intervals.s.min = value

    def setMax(prop: String, value: Int) = prop match
      case "x" => intervals.x.max = value
      case "m" => intervals.m.max = value
      case "a" => intervals.a.max = value
      case "s" => intervals.s.max = value

    def clone(): Intervals =
      Intervals(
        Interval(intervals.x.min, intervals.x.max),
        Interval(intervals.m.min, intervals.m.max),
        Interval(intervals.a.min, intervals.a.max),
        Interval(intervals.s.min, intervals.s.max),
      )

  extension (cond: Cond)
    def negated(): Cond = cond.cond match
      case Condition.Lss => Cond(cond.prop, Condition.Gtr, cond.value - 1)
      case Condition.Gtr => Cond(cond.prop, Condition.Lss, cond.value + 1)

  extension (conds: Seq[Cond])
    def toInterval(): Intervals =
      val intervals = Intervals(
        Interval(1, 4000),
        Interval(1, 4000),
        Interval(1, 4000),
        Interval(1, 4000),
      )

      for cond <- conds do
        cond.cond match {
          case Condition.Lss => intervals.setMax(cond.prop, cond.value - 1)
          case Condition.Gtr => intervals.setMin(cond.prop, cond.value + 1)
        }

      intervals

  def acceptedConditions(
    workflows: Map[String, Workflow],
  ) = {
    val accepted = ArrayBuffer[Seq[Cond]]()
    val stack = Stack[(String, Seq[Cond])]()
    stack.push(("in", Seq()))

    while (!stack.isEmpty) {
      val (state, matched) = stack.pop()
      if (state == "A") {
        accepted += matched
      } else if (state != "R") {
        val wf = workflows(state)
        var conds = matched.toSeq

        for (rule <- wf.rules) {
          val cond = Cond(rule.prop, rule.cond, rule.value)
          val neg = cond.negated()
          stack.push((rule.target, conds :+ cond))
          conds = conds :+ neg
        }

        stack.push((wf.fallback, conds))
      }
    }

    accepted
  }

  def part2(input: String = "input.txt") =
    val (workflows, _) = parseInput(input)
    val conditions = acceptedConditions(workflows)
    val intervals = conditions.map(_.toInterval())
    intervals.map(i => i.x.len() * i.m.len() * i.a.len() * i.s.len()).sum
}

println(Day19.part1())
println(Day19.part2())
