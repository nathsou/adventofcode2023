import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Map, Set, Stack}

object Day22 {
  case class Vec3(x: Int, y: Int, z: Int)
  case class Brick(from: Vec3, to: Vec3, id: Int)
  case class Relations(supports: Map[Int, Set[Int]], supportedBy: Map[Int, Set[Int]])

  def parseInput(input: String) =
    var index = 0
    Source.fromFile(input).getLines().map(line => line match {
      case s"${from}~${to}" => {
        val f = from.split(",").map(_.toInt)
        val t = to.split(",").map(_.toInt)
        index += 1
        Brick(Vec3(f(0), f(1), f(2)), Vec3(t(0), t(1), t(2)), index)
      }
    }).toSeq

  extension (b: Brick)
    def intersects(other: Brick): Boolean =
      val x = b.from.x <= other.to.x && b.to.x >= other.from.x
      val y = b.from.y <= other.to.y && b.to.y >= other.from.y
      val z = b.from.z <= other.to.z && b.to.z >= other.from.z
      x && y && z

    def movedDown(deltaZ: Int): Brick =
      Brick(
        from = Vec3(b.from.x, b.from.y, b.from.z - deltaZ),
        to = Vec3(b.to.x, b.to.y, b.to.z - deltaZ),
        id = b.id
      )

    def supportedBy(stack: Stack[Brick]) =
      val moved = b.movedDown(1)
      stack.toSeq.filter(_.intersects(moved))

    def fallDown(stack: Stack[Brick]): Brick =
      var lastValid = b
      var z = 1
      while z < b.from.z do
        val moved = b.movedDown(z)
        val intersects = stack.exists(_.intersects(moved))
        if intersects then
          return lastValid
        else
          lastValid = moved

        z += 1
        
      lastValid

  def fallDown(bricks: Seq[Brick]) =
    val sortedZ = bricks.sortBy(_.from.z)
    val stack = Stack[Brick]()

    for brick <- sortedZ do
      if stack.isEmpty then
        stack.push(brick)
      else {
        val fallen = brick.fallDown(stack)
        stack.push(fallen)
      }
  
    stack

  extension (stack: Stack[Brick])
    def relations(): Relations =
      val supports = Map[Int, Set[Int]]()
      val supportedBy = Map[Int, Set[Int]]()

      for brick <- stack do
        supports += (brick.id -> Set[Int]())
        supportedBy += (brick.id -> Set[Int]())

      for brick <- stack do
        val supported = brick.supportedBy(stack).map(_.id)
        for supportedLabel <- supported do
          if supportedLabel != brick.id then
            supports(supportedLabel) += brick.id
            supportedBy(brick.id) += supportedLabel

      Relations(supports, supportedBy)
    
  def part1(input: String = "input.txt") =
    val bricks = parseInput(input)
    val stack = fallDown(bricks)
    val relations = stack.relations()
    val supports = relations.supports
    val supportedBy = relations.supportedBy

    def canBeDisintegrated(id: Int): Boolean =
      supports(id).forall(l => {
        val supported = supportedBy(l)
        supported.size > 1
      })

    stack.count(b => canBeDisintegrated(b.id))

  def part2(input: String = "input.txt") =
    val bricks = parseInput(input)
    val stack = fallDown(bricks)
      val relations = stack.relations()
    val supports = relations.supports
    val supportedBy = relations.supportedBy

    def countFallingBricks(id: Int, removed: Set[Int]): Int = {
      var count = 0
      removed += id
      
      for supportedLabel <- supports(id) do
        if supportedBy(supportedLabel).diff(removed).isEmpty then
          count += 1 + countFallingBricks(supportedLabel, removed)

      count
    }

    stack.map(b => countFallingBricks(b.id, Set())).sum
}

println(Day22.part1())
println(Day22.part2())
