package ad

import zio.ZIO


sealed trait ParserError extends Throwable
case object EmptyTree extends ParserError
case object InvalidTreeShape extends ParserError
case class CannotParse(message: String) extends ParserError

object Parser:
  def parse(lines: List[String]): ZIO[Any, ParserError, Node] =
    for {
        integers <- asIntegers(lines)
        tree <- asTree(integers)
      } yield tree

  def prepend(trees: Vector[Node], previousLayer: Vector[Int]) =
    if previousLayer.length * 2 != trees.length then
      ZIO.fail(InvalidTreeShape)
    else
      ZIO.foreach(previousLayer zip trees.grouped(2)) {
        case (parent, Vector(left, right)) =>
          ZIO.succeed(Node(parent, left, right))
        case (parent, _) =>
          // This case should never happen due to previous size check, but we need to show compiler that it is handled
          ZIO.fail(InvalidTreeShape)
      }

  def asTree(value: List[Vector[Int]]) =
    value.reverse match
      case head :: tail => ZIO.foldLeft(tail)(head.map(Node(_)))(prepend).flatMap {
        case Vector(onlyTree) => ZIO.succeed(onlyTree)
        case _ => ZIO.fail(InvalidTreeShape)
      }
      case Nil => ZIO.fail(EmptyTree)

  def asIntegers(lines: List[String]) =
    ZIO.foreach(lines) {
      line =>
        ZIO.foreach(line.split(" ").toVector) {
          number =>
            ZIO.fromEither(number.toIntOption.toRight(CannotParse(number)))
        }
    }