package ad.library

import zio.test.Assertion.*
import zio.test.*


object GraphSpec extends ZIOSpecDefault:
  def spec = suite("Graph computation spec"):
    test("should handle simple routing"):
      val graph = Graph.apply(Vector(
        Vector(7),
        Vector(6, 3),
        Vector(3, 8, 5),
        Vector(11, 2, 10,  9)
      )).getOrElse(???)

      assertTrue(graph.shortestPath == List(7, 6, 3, 2))

