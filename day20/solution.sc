import scala.io.Source
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.Map
import scala.collection.mutable.Queue
import scala.collection.mutable.Set

object Day20 {
  case class Box(var state: Boolean, var initialized: Boolean = false)

  enum Module:
    case FlipFlop(name: String, state: Box)
    case Broadcaster
    case Conjunction(name: String, state: Box, states: Map[String, Boolean])

  case class Connection(from: String, to: Seq[String])

  def parseConnection(line: String): (Module, Connection) = line match
    case s"${from} -> ${to}" => {
      val con =  Connection(from, to.split(", "))
      val mod = from match {
        case s"%$name" => Module.FlipFlop(name, Box(false))
        case s"&$name" => Module.Conjunction(name, Box(false), Map[String, Boolean]())
        case _ => Module.Broadcaster
      }

      (mod, con)
    }

  def parseInput(input: String) =
    val modules = ArrayBuffer[Module]()
    val connections = Map[String, (Module, Seq[String])]()

    val lines = Source.fromFile(input).getLines()

    for line <- lines do
      val (mod, con) = parseConnection(line)
      modules += mod
      connections += (mod.name() -> (mod, con.to))

    for mod <- modules do
      mod match {
        case Module.Conjunction(name, state, states) => {
          // get all connections to this conjunction
          val conns = connections.filter((k, v) => v._2.contains(name))
          // set the initial state of the conjunction to false
          for (k, _) <- conns do
            states += (k -> false)
        }
        case _ => ()
      }

    (modules, connections)

  extension (m: Module)
    def name(): String = m match
      case Module.FlipFlop(name, _) => name
      case Module.Conjunction(name, _, _) => name
      case Module.Broadcaster => "broadcaster"
    
    def str(): String = {
      m match {
        case Module.FlipFlop(name, state) => s"FF($name, ${state.state})"
        case Module.Conjunction(name, state, _) => s"C($name, ${state.state})"
        case Module.Broadcaster => "B"
      }
    }

    def state(): Boolean = m match
      case Module.FlipFlop(_, state) => state.state
      case Module.Conjunction(_, state, _) => state.state
      case _ => false
    
    def isBroadcaster(): Boolean = m match
      case Module.Broadcaster => true
      case _ => false
    
    def isConjunction(): Boolean = m match
      case Module.Conjunction(_, _, _) => true
      case _ => false

  def simulate(presses: Long, connections: Map[String, (Module, Seq[String])], conjs: Map[String, Long]) = {
    val queue = Queue[(String, String, Boolean)]()
    queue.enqueue(("broadcaster", "button", false))
    var lowPulses = 0L
    var highPulses = 0L

    while (!queue.isEmpty) {
      val (name, source, input) = queue.dequeue()

      if (name == "rx" && !input) {
        throw new Exception(s"rx at $presses")
      }

      if (input) {
        highPulses += 1L
      } else {
        lowPulses += 1L
      }

      if (connections.contains(name)) {
        val mod = connections(name)._1

        mod match {
          case Module.Broadcaster => {
            for (to <- connections(name)._2) {
              queue.enqueue((to, name, input))
            }
          }
          case Module.FlipFlop(name, state) => {
            if (!input) {
              val newState = !state.state
                state.state = newState
                state.initialized = true
  
                for (to <- connections(name)._2) {
                  queue.enqueue((to, name, newState))
                }
            }
          }
          case Module.Conjunction(name, state, states) => {
            // Conjunction modules (prefix &) remember the type of the most recent pulse received from each of their connected input modules; they initially default to remembering a low pulse for each input. When a pulse is received, the conjunction module first updates its memory for that input. Then, if it remembers high pulses for all inputs, it sends a low pulse; otherwise, it sends a high pulse.

            states(source) = input
            val newState = !states.values.forall(v => v)

            if (!newState && !conjs.contains(name)) {
              // println(s"conjunction $name at $presses with $states")
              conjs += (name -> presses)
            }

            state.state = newState
            state.initialized = true
            
            for (to <- connections(name)._2) {
              queue.enqueue((to, name, newState))
            }
          }
        }
      }
    }
    
    // println(s"state: ${connections.map((k, v) => s"${k} -> ${v._1.str()}").mkString(", ")}")
    // println(s"low: $lowPulses, high: $highPulses")
    (lowPulses, highPulses)
  }
    
  def part1(input: String = "input.txt") = {
    val (modules, connections) = parseInput(input)
    val conjs = Map[String, Long]()
    var lowPulses = 0L
    var highPulses = 0L
    var i = 1L

    for (i <- 0L until 1000L) {
      val (low, high) = simulate(i, connections, conjs)
      lowPulses += low
      highPulses += high
    }

    lowPulses * highPulses
  }

  def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)
  def llcm(a: Long, b: Long): Long = (a * b) / gcd(a, b)
  def llcm(ns: Iterable[Long]): Long = ns.reduce((a, b) => llcm(a, b))

  def part2(input: String = "input.txt") = {
    val (modules, connections) = parseInput(input)
    val conjs = Map[String, Long]()

    for (i <- 0L to 10_000L) {
      simulate(i, connections, conjs)
    }

    for ((k, v) <- conjs) {
      if (v == 0L) {
        conjs.remove(k)
      }
    }

    val remainingConjs = Set.from(modules.filter(m => m.isConjunction() && (!conjs.contains(m.name()))).map(_.name()))

    println(remainingConjs)

    while (remainingConjs.size > 0) {
      for (conj <- remainingConjs) {
        val mod = connections(conj)._1
        mod match {
          case Module.Conjunction(name, state, states) => {
            if (states.keys.forall(k => conjs.contains(k))) {
              val cycles = states.keys.map(k => conjs(k))
              val cycle = llcm(cycles)
              conjs += (name -> cycle)
              remainingConjs.remove(name)
            }
          }
          case _ => ()
        }
      }
    }

    println(conjs)
    println(remainingConjs)
  }
}

println(Day20.part1("input.txt"))
// println(Day20.part2("input.txt"))

// 11752068000 -> too low
