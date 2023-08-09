package ad

import zio.{ZIO, ZIOAppDefault}

object Main extends ZIOAppDefault :

  def run =
    for {
      input <- readAllInput()
      graph <- Parser.parse(input)
      _ <- zio.Console.printLine("Input received, crunching the path")
      _ <- zio.Console.printLine(System.currentTimeMillis())
      _ <- zio.Console.printLine(graph.shortestPath)
      _ <- zio.Console.printLine(System.currentTimeMillis())
    } yield ()


  def readAllInput(acc: List[String] = Nil): ZIO[Any, Nothing, List[String]] =
    zio.Console.readLine.either.flatMap {
      case Left(_) => ZIO.succeed(acc)
      case Right(value) => readAllInput(acc :+ value)
    }
