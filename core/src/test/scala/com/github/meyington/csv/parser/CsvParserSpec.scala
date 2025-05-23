package com.github.meyington.csv.parser

import zio.*
import zio.test.*
import zio.test.Assertion.*

object CsvParserSpec extends ZIOSpecDefault {

  class TestCsvParser extends CsvParser {
    override def parseLine(line: String): IO[CsvError, CsvRow] = {
      if (line.isEmpty) ZIO.fail(CsvError.ParseError("Empty line"))
      else if (line == "invalid") ZIO.fail(CsvError.ValidationError("Invalid data"))
      else ZIO.succeed(CsvRow(line.split(",").toVector))
    }

    override def parseLines(lines: Seq[String]): IO[CsvError, Vector[CsvRow]] =
      ZIO.foreach(lines.toVector)(parseLine)
  }

  def spec: Spec[Any, CsvError] = suite("CsvParser")(
    suite("parseLine")(
      test("Успешный парсинг строки") {
        for {
          parser <- ZIO.succeed(new TestCsvParser)
          result <- parser.parseLine("a,b,c")
        } yield assert(result)(equalTo(CsvRow(Vector("a", "b", "c"))))
      },
      test("Обработка пустой строки как ошибки") {
        for {
          parser <- ZIO.succeed(new TestCsvParser)
          result <- parser.parseLine("").exit
        } yield assert(result)(fails(isSubtype[CsvError.ParseError](anything)))
      },
      test("Обработка ошибки валидации") {
        for {
          parser <- ZIO.succeed(new TestCsvParser)
          result <- parser.parseLine("invalid").exit
        } yield assert(result)(fails(isSubtype[CsvError.ValidationError](anything)))
      }
    ),

    suite("parseLines")(
      test("Успешный парсинг нескольких строк") {
        for {
          parser <- ZIO.succeed(new TestCsvParser)
          result <- parser.parseLines(Seq("a,b", "c,d"))
        } yield assert(result)(equalTo(Vector(
          CsvRow(Vector("a", "b")),
          CsvRow(Vector("c", "d"))
        )))
      },
      test("Обработка прерывания парсинга при первой ошибке") {
        for {
          parser <- ZIO.succeed(new TestCsvParser)
          result <- parser.parseLines(Seq("a,b", "", "c,d")).exit
        } yield assert(result)(fails(isSubtype[CsvError.ParseError](anything)))
      }
    )
  )
}
