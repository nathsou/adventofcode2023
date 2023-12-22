import scala.io.Source
import scala.collection.mutable.{ArrayBuffer, Map, Set, Stack}
import os.remove

object Day22 {
  case class Vec3(x: Int, y: Int, z: Int)
  case class Brick(from: Vec3, to: Vec3, label: String)

  def parseInput(input: String) =
    var index = 0
    Source.fromFile(input).getLines().map(line => line match {
      case s"${from}~${to}" => {
        val f = from.split(",").map(_.toInt)
        val t = to.split(",").map(_.toInt)
        val label = s"${index}"
        index += 1
        Brick(Vec3(f(0), f(1), f(2)), Vec3(t(0), t(1), t(2)), label)
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
        label = b.label
      )

    def supportedBy(stack: Stack[Brick]) =
      val moved = b.movedDown(1)
      stack.toSeq.filter(_.intersects(moved))

    def fallDown(stack: Stack[Brick]): Brick = {
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
    }

  def fallDown(bricks: Seq[Brick]) = {
    // sort the bricks by min z
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
  }
    
  def part1(input: String = "input.txt") =
    val bricks = parseInput(input)
    val stack = fallDown(bricks)
    val supports = Map[String, Set[String]]()
    val supportedBy = Map[String, Set[String]]()

    for brick <- stack do
      supports += (brick.label -> Set[String]())
      supportedBy += (brick.label -> Set[String]())

    for brick <- stack do
      val supported = brick.supportedBy(stack).map(_.label)
      for supportedLabel <- supported do
        supports(supportedLabel) += brick.label
        supportedBy(brick.label) += supportedLabel

    for (label, supported) <- supports do
      if supported.contains(label) then supported.remove(label)

    // for (label, supported) <- supports do
    //   println(s"$label is supporting ${supported.mkString(", ")}")

    // println()

    for (label, supported) <- supportedBy do
      if supported.contains(label) then supported.remove(label)

    // for (label, supported) <- supportedBy do
    //   println(s"$label is supported by ${supported.mkString(", ")}")

    def canBeDisintegrated(label: String): Boolean =
      supports(label).forall(l => {
        val supported = supportedBy(l)
        supported.size > 1
      })

    // println()

    var count = 0
    for brick <- stack do
      if canBeDisintegrated(brick.label) then
        count += 1
        // println(s"${brick.label} can be disintegrated")
      

    count

  def part2(input: String = "input.txt") =
    val bricks = parseInput(input)
    val stack = fallDown(bricks)
    val supports = Map[String, Set[String]]()
    val supportedBy = Map[String, Set[String]]()

    for brick <- stack do
      supports += (brick.label -> Set[String]())
      supportedBy += (brick.label -> Set[String]())

    for brick <- stack do
      val supported = brick.supportedBy(stack).map(_.label)
      for supportedLabel <- supported do
        supports(supportedLabel) += brick.label
        supportedBy(brick.label) += supportedLabel

    for (label, supported) <- supports do
      if supported.contains(label) then supported.remove(label)

    // for (label, supported) <- supports do
    //   println(s"$label is supporting ${supported.mkString(", ")}")

    println()

    for (label, supported) <- supportedBy do
      if supported.contains(label) then supported.remove(label)

    // for (label, supported) <- supportedBy do
    //   println(s"$label is supported by ${supported.mkString(", ")}")
    
    // println()

    def canBeDisintegrated(label: String): Boolean =
      supports(label).forall(l => {
        val supported = supportedBy(l)
        supported.size > 1
      })

    def countFallingBricks(label: String, removed: Set[String]): Int = {
      var count = 0
      removed += label
      for supportedLabel <- supports(label) do
        val rem = supportedBy(supportedLabel).diff(removed)
        // println(s"$supportedLabel is supported by ${rem.mkString(", ")}")

        if rem.isEmpty then
          count += 1 + countFallingBricks(supportedLabel, removed)

      count
    }

    stack.map(b => countFallingBricks(b.label, Set[String]())).sum

    // for brick <- stack do
    //   println(s"${brick.label} -> ${countFallingBricks(brick.label, Set[String]())}")
    //   println()
}

// println(Day22.part1("input.txt"))
println(Day22.part2("input.txt"))
