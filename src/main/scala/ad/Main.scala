package ad

import zio.{ZIO, ZIOAppDefault}

object Main extends ZIOAppDefault :

  def run =
    for {
      input <- readAllInput()
      _ <- zio.Console.printLine(input)
    } yield ()


  def readAllInput(acc: List[String] = Nil): ZIO[Any, Nothing, List[String]] =
    zio.Console.readLine.either.flatMap {
      case Left(_) => ZIO.succeed(acc)
      case Right(value) => readAllInput(acc :+ value)
    }
