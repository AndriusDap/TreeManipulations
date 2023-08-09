package ad

import scala.annotation.tailrec

case class Graph(weights: Vector[Vector[Int]]):
  def shortestPath  =
    val (distances, previous) = bfDijkstra(Map((0, 0) -> 0), Map.empty, (0, 0))
    val lastRow = weights.size - 1

    val target = distances.filter(_._1._1 == lastRow).minBy(_._2)
    reconstructPath(target._1, previous).map(weight).reverse


  def reconstructPath(to: (Int, Int), previous: Map[(Int, Int), (Int, Int)]): List[(Int, Int)] =
    reconstructPath(to, previous, List(to))

  def reconstructPath(to: (Int, Int), previous: Map[(Int, Int), (Int, Int)], path: List[(Int, Int)]): List[(Int, Int)] =
    previous.get(to) match
      case Some(next) => reconstructPath(next, previous, path :+ next)
      case None => path


  def neighbours(target: (Int, Int)): Vector[(Int, Int)] =
    val (x, y) = target
    Vector((x + 1, y), (x + 1, y + 1)).filterNot((x, y) => weights.lift(x).flatMap(_.lift(y)).isEmpty)


  def weight(target: (Int, Int)): Int = weights(target._1)(target._2)

  @tailrec
  final def bfDijkstra(
    distances: Map[(Int, Int), Int],
    previous: Map[(Int, Int), (Int, Int)],
    current: (Int, Int)
  ): (Map[(Int, Int), Int], Map[(Int, Int), (Int, Int)]) =

    if weights.lift(current._1).flatMap(w => w.lift(current._2)).isEmpty then
      (distances, previous)
    else
      val updates = for {
        neighbour <- neighbours(current)
        updatedDistance = distances(current) + weight(neighbour)
      } yield distances.get(neighbour) match
        case None => Some(neighbour -> (updatedDistance, current))
        case Some(d) if d > updatedDistance => Some(neighbour -> (updatedDistance, current))
        case _ => None

      val distanceUpdates = updates.flatten.map { case (node, (newDistance, _)) => node -> newDistance }.toMap
      val previousUpdates = updates.flatten.map { case (node, (_, from)) => node -> from }.toMap

      val nextNode = if weights(current._1).length == current._2 + 1 then
        (current._1 + 1, 0)
      else
        (current._1, current._2 + 1)

      bfDijkstra(distances ++ distanceUpdates, previous ++ previousUpdates, nextNode)

object Graph:
  sealed trait GraphError extends Throwable
  case object InvalidGraph extends GraphError
  case object EmptyGraph extends GraphError
  def apply(weights: Vector[Vector[Int]]): Either[GraphError, Graph] =
    if weights.isEmpty then
      Left(EmptyGraph)
    else
      weights.zipWithIndex.find((items, index) => items.size != index + 1) match
        case Some(_) => Left(InvalidGraph)
        case None => Right(new Graph(weights))