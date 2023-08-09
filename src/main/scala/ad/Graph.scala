package ad

import scala.collection.immutable.{AbstractSet, SortedSet}

case class Graph(weights: Vector[Vector[Int]]):
  def shortestPath  =
    val allNodes = weights.zipWithIndex.flatMap {
      case (nodes, x) => nodes.indices.map(y => (x, y))
    }
    val (distances, previous) = dijkstra(Map((0, 0) -> 0), Map.empty, allNodes.toSet)
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
  def dijkstra(
    distances: Map[(Int, Int), Int],
    previous: Map[(Int, Int), (Int, Int)],
    unsolved: Set[(Int, Int)]
  ): (Map[(Int, Int), Int], Map[(Int, Int), (Int, Int)]) =
    given optionalIntOrdering: Ordering[Option[Int]] with {
      override def compare(x: Option[Int], y: Option[Int]): Int =
        (x, y) match
          case (Some(x), Some(y)) => x.compare(y)
          case (Some(_), None) => -1
          case (None, Some(_)) => 1
          case (None, None) => 0
    }

    if unsolved.isEmpty then
      (distances, previous)
    else
      // TODO: Verify that ordering works here
      val next = unsolved.minBy(element => distances.get(element))
      val updates: Seq[Option[((Int, Int), (Int, (Int, Int)))]] = for {
        neighbour <- neighbours(next).filter(unsolved.contains)
        updatedDistance = distances(next) + weight(neighbour)
      } yield distances.get(neighbour) match
        case None => Some(neighbour -> (updatedDistance, next))
        case Some(d) if d > updatedDistance => Some(neighbour -> (updatedDistance, next))
        case _ => None


      dijkstra(
        updates.flatten.foldLeft(distances){
          case (distances, (node, (newDistance, _))) =>
            distances + (node -> newDistance)
        },
        updates.flatten.foldLeft(previous){
          case (previous, (node, (_, from))) =>
            previous + (node -> from)
        },
        unsolved - next
      )


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