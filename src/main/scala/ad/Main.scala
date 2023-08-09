package ad

import zio.{ZIO, ZIOAppDefault}

object Main extends ZIOAppDefault :

  def run =
    for {
      input <- readAllInput()
      graph <- Parser.parse(input)
      //start = System.currentTimeMillis()
      shortestPath = graph.shortestPath
      //_ <- zio.Console.printLine(System.currentTimeMillis() - start)
      _ <- zio.Console.printLine(s"Minimal path is: ${shortestPath.mkString(" + ")} = ${shortestPath.sum}")
    } yield ()


  def readAllInput(acc: List[String] = Nil): ZIO[Any, Nothing, List[String]] =
    zio.Console.readLine.either.flatMap {
      case Left(_) => ZIO.succeed(acc)
      case Right(value) => readAllInput(acc :+ value)
    }
