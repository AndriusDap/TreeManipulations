package ad

import ad.library.Graph
import zio.ZIO


sealed trait ParserError extends Throwable
case class CannotParse(message: String) extends ParserError

object Parser:
  def wholeInput(acc: List[String] = Nil): ZIO[Any, Nothing, List[String]] =
    zio.Console.readLine.either.flatMap:
      case Left(_) => ZIO.succeed(acc)
      case Right(value) => wholeInput(acc :+ value)

  def parse(lines: List[String]) =
    for integers <- asIntegers(lines)
        tree <- ZIO.fromEither(Graph.apply(integers.toVector))
    yield tree

  def asIntegers(lines: List[String]) =
    ZIO.foreach(lines):
      line =>
        ZIO.foreach(line.split(" ").toVector):
          number =>
            ZIO.fromEither(number.toIntOption.toRight(CannotParse(number)))