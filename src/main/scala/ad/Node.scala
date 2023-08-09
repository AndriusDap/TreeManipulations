package ad

case class Node(
   weight: Int,
   left: Option[Node],
   right: Option[Node]
)

object Node:
  def apply(weight: Int): Node = Node(weight, None, None)
  def apply(weight: Int, left: Node, right: Node): Node = Node(weight, Some(left), Some(right))
