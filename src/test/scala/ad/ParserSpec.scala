package ad

import ad.library.Graph
import ad.library.Graph.{EmptyGraph, InvalidGraph}
import zio.test.*
import zio.test.Assertion.*
import zio.test.ZIOSpecDefault

object ParserSpec extends ZIOSpecDefault:
  def spec = suite("Parser spec")(
    test("Single value should return a simple node"):
      for result <- Parser.parse(List("5")).either
      yield assertTrue(result == Graph(Vector(Vector(5)))),

    test("simple tree should be parsed"):
      for result <- Parser.parse(List("5", "1 2")).either
      yield assertTrue(result == Graph(Vector(Vector(5), Vector(1, 2)))),

    test("Non integer should fail"):
      for result <- Parser.parse(List("banana")).either
      yield assertTrue(result == Left(CannotParse("banana"))),

    test("Empty list should return some failure"):
      for result <- Parser.parse(List.empty).either
      yield assertTrue(result == Left(EmptyGraph)),

    test("Invalid shape should be reported"):
      for result <- Parser.parse(List("1 2 3")).either
      yield assertTrue(result == Left(InvalidGraph)),

    test("Invalid shape should be reported"):
      for result <- Parser.parse(List("1", "1 2 3")).either
      yield assertTrue(result == Left(InvalidGraph)),


    test("Invalid shape should be reported"):
      for result <- Parser.parse(List("1", "1 2", "1")).either
      yield assertTrue(result == Left(InvalidGraph))
  )
