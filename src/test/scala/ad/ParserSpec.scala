package ad

import zio.test._
import zio.test.Assertion._
import zio.test.ZIOSpecDefault

object ParserSpec extends ZIOSpecDefault {

    def spec = suite("Parser spec")(
      test("Single value should return a simple node") {
        for {
          result <- Parser.parse(List("5"))
        } yield assertTrue(result == Node(5))
      },
      test("simple tree should be parsed") {
        for {
          result <- Parser.parse(List("5", "1 2"))
        } yield assertTrue(result == Node(5, Node(1), Node(2)))
      },
      test("Non integer should fail") {
        for {
          result <- Parser.parse(List("banana")).either
        } yield assertTrue(result == Left(CannotParse("banana")))
      },
      test("Empty list should return some failure") {
        for {
          result <- Parser.parse(List.empty).either
        } yield assertTrue(result == Left(EmptyTree))
      },
      test("Invalid shape should be reported") {
        for {
          result <- Parser.parse(List("1 2 3")).either
        } yield assertTrue(result == Left(InvalidTreeShape))
      },
      test("Invalid shape should be reported") {
        for {
          result <- Parser.parse(List("1", "1 2 3")).either
        } yield assertTrue(result == Left(InvalidTreeShape))
      },
      test("Invalid shape should be reported") {
        for {
          result <- Parser.parse(List("1", "1 2", "1")).either
        } yield assertTrue(result == Left(InvalidTreeShape))
      }

    )
}
