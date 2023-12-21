import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Map, Queue, Set}

object Day20 {
  case class Box(var ref: Boolean)

  enum Module:
    case FlipFlop(name: String, state: Box)
    case Broadcaster
    case Conjunction(name: String, state: Box, states: Map[String, Boolean])

  case class Connection(from: String, to: Seq[String])

  def parseConnection(line: String): (Module, Connection) = line match
    case s"${from} -> ${to}" => {
      val con = Connection(from, to.split(", "))
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
          // set the initial state of the conjunction to off
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
    
    def isConjunction(): Boolean = m match
      case Module.Conjunction(_, _, _) => true
      case _ => false

  def simulate(connections: Map[String, (Module, Seq[String])], buttonPresses: Long) = {
    val queue = Queue[(String, String, Boolean)]()
    val conjCycles = Map[String, Long]()
    var lowPulses = 0L
    var highPulses = 0L
    
    for (presses <- 1L to buttonPresses) {
      queue.enqueue(("broadcaster", "button", false))

      while (!queue.isEmpty) {
        val (name, source, input) = queue.dequeue()

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
                val newState = !state.ref
                  state.ref = newState
    
                  for (to <- connections(name)._2) {
                    queue.enqueue((to, name, newState))
                  }
              }
            }
            case Module.Conjunction(name, state, states) => {
              states(source) = input
              val newState = !states.values.forall(v => v)

              if (!newState && newState != state.ref && !conjCycles.contains(name)) {
                conjCycles += (name -> presses)
              }

              state.ref = newState
              
              for (to <- connections(name)._2) {
                queue.enqueue((to, name, newState))
              }
            }
          }
        }
      }
    }

    (lowPulses, highPulses, conjCycles)
  }
    
  def part1(input: String = "input.txt") =
    val (modules, connections) = parseInput(input)
    val (low, high, _) = simulate(connections, 1000L);
    low * high

  def gcd(a: Long, b: Long): Long = if b == 0 then a else gcd(b, a % b)
  def llcm(a: Long, b: Long): Long = (a * b) / gcd(a, b)
  def llcm(ns: Iterable[Long]): Long = ns.reduce((a, b) => llcm(a, b))

  def part2(input: String = "input.txt") =
    val (modules, connections) = parseInput(input)
    val (_, _, conjs) = simulate(connections, 5000L)

    val remainingConjs = Set.from(
      modules.filter(m => m.isConjunction() && !conjs.contains(m.name())).map(_.name())
    )

    while (remainingConjs.size > 0) {
      for (conj <- remainingConjs) {
        val (mod, _) = connections(conj)
        mod match {
          case Module.Conjunction(name, state, states) => {
            if (states.keys.forall(k => conjs.contains(k))) {
              val cycles = states.keys.map(k => conjs(k))
              conjs += (name -> llcm(cycles))
              remainingConjs.remove(name)
            }
          }
          case _ => ()
        }
      }
    }

    conjs(connections.find(_._2._2.contains("rx")).get._1)
}

println(Day20.part1())
println(Day20.part2())
