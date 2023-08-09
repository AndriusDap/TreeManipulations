package ad

import zio.ZIOAppDefault

object Main extends ZIOAppDefault:
  def run =
    for input <- Parser.wholeInput()
        start = System.currentTimeMillis()
        graph <- Parser.parse(input)
        shortestPath = graph.shortestPath
        _ <- zio.Console.printLine(s"Minimal path is: ${shortestPath.mkString(" + ")} = ${shortestPath.sum}")
        _ <- zio.Console.printLine(s"Duration is ${System.currentTimeMillis() - start}")
    yield ()


