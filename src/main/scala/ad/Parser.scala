package ad

import zio.ZIO


sealed trait ParserError extends Throwable
case class CannotParse(message: String) extends ParserError

object Parser:
  def parse(lines: List[String]) =
    for {
        integers <- asIntegers(lines)
        tree <- ZIO.fromEither(Graph.apply(integers.toVector))
      } yield tree

  def asIntegers(lines: List[String]) =
    ZIO.foreach(lines) {
      line =>
        ZIO.foreach(line.split(" ").toVector) {
          number =>
            ZIO.fromEither(number.toIntOption.toRight(CannotParse(number)))
        }
    }