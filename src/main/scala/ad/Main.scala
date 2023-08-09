package ad

import zio.ZIOAppDefault

object Main extends ZIOAppDefault:
  def run =
    for input <- Parser.wholeInput()
        graph <- Parser.parse(input)
        shortestPath = graph.shortestPath
        _ <- zio.Console.printLine(s"Minimal path is: ${shortestPath.mkString(" + ")} = ${shortestPath.sum}")
    yield ()


