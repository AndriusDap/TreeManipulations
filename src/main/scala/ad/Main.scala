package ad

import zio.ZIOAppDefault

object Main extends ZIOAppDefault:
  def run =
    for input <- Parser.wholeInput()
        maybeGraph <- Parser.parse(input).either
        _ <- maybeGraph match
          case Left(error) =>
             zio.Console.printError(s"Error occurred while reading the input - ${error.toString}\n")
          case Right(graph) =>
            val shortestPath = graph.shortestPath
            zio.Console.printLine(s"Minimal path is: ${shortestPath.mkString(" + ")} = ${shortestPath.sum}")
        yield ()


