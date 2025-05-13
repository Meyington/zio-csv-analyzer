package com.github.meyington.csv.parser

import zio.*
import zio.test.*
import zio.test.Assertion.*

object CsvParserImplSpec extends ZIOSpecDefault {
  def spec = suite("CsvParserImpl")(
    suite("ParseLine с настройками по умолчанию")(
      test("Парсинг простой строки без специальных символов") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLine("a,b,c")
        } yield assert(result)(equalTo(CsvRow(Vector("a", "b", "c"))))
      },

      test("Парсинг строки с пустыми значениями") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLine("a,,c")
        } yield assert(result)(equalTo(CsvRow(Vector("a", "", "c"))))
      },

      test("Парсинг значений в кавычках") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLine("""one,"two,with,commas",three""")
        } yield assert(result)(equalTo(CsvRow(Vector("one", "two,with,commas", "three"))))
      },

      test("Парсинг экранированных символов") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLine("""normal,with\"quote,with\,comma""")
        } yield assert(result)(equalTo(CsvRow(Vector("normal", "with\"quote", "with,comma"))))
      },

      test("Обработка ошибки при незакрытых кавычках") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLine("""one,"unclosed""").exit
        } yield assert(result)(fails(isSubtype[CsvError.ParseError](anything)))
      },

      test("Обработка ошибки при неполном экранировании") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLine("""one,incomplete\""").exit
        } yield assert(result)(fails(isSubtype[CsvError.ParseError](anything)))
      }
    ),

    suite("ParseLine с пользовательской конфигурацией")(
      test("Парсинг с использованием точки с запятой как разделителя") {
        val config = new CsvConfig {
          val delimiter: Char = ';'
          val quoteChar: Char = '"'
          val escapeChar: Char = '\\'
        }
        for {
          parser <- ZIO.succeed(CsvParserImpl(config))
          result <- parser.parseLine("a;b;c")
        } yield assert(result)(equalTo(CsvRow(Vector("a", "b", "c"))))
      }
    ),

    suite("ParseLines")(
      test("Парсинг нескольких строк без специальных символов") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLines(Seq("a,b", "c,d", "e,f"))
        } yield assert(result)(equalTo(Vector(
          CsvRow(Vector("a", "b")),
          CsvRow(Vector("c", "d")),
          CsvRow(Vector("e", "f"))
        )))
      },

      test("Обработка прерывания при ошибке парсинга") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLines(Seq("a,b", """c,"unclosed""", "e,f")).exit
        } yield assert(result)(fails(isSubtype[CsvError.ParseError](anything)))
      }
    )
  )
} 