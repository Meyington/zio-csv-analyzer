package com.github.meyington.csv.parser

import zio.*
import zio.test.*
import zio.test.Assertion.*

object CsvParserImplSpec extends ZIOSpecDefault {
  def spec = suite("CsvParserImpl")(
    suite("ParseLine с настройками по умолчанию")(
      test("Простая строка без специальных символов") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLine("a,b,c")
        } yield assert(result)(equalTo(CsvRow(Vector("a", "b", "c"))))
      },

      test("Строка с пустыми значениями") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLine("a,,c")
        } yield assert(result)(equalTo(CsvRow(Vector("a", "", "c"))))
      },

      test("Значения в кавычках") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLine("""one,"two,with,commas",three""")
        } yield assert(result)(equalTo(CsvRow(Vector("one", "two,with,commas", "three"))))
      },

      test("Экранированные символы") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLine("""normal,with\"quote,with\,comma""")
        } yield assert(result)(equalTo(CsvRow(Vector("normal", "with\"quote", "with,comma"))))
      },

      test("Ошибка при незакрытых кавычках") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLine("""one,"unclosed""").exit
        } yield assert(result)(fails(isSubtype[CsvError.ParseError](anything)))
      },

      test("Ошибка при неполном экранировании") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLine("""one,incomplete\""").exit
        } yield assert(result)(fails(isSubtype[CsvError.ParseError](anything)))
      }
    ),

    suite("ParseLine с пользовательской конфигурацией")(
      test("Использование точки с запятой как разделителя") {
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
      test("Несколько строк без специальных символов") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLines(Seq("a,b", "c,d", "e,f"))
        } yield assert(result)(equalTo(Vector(
          CsvRow(Vector("a", "b")),
          CsvRow(Vector("c", "d")),
          CsvRow(Vector("e", "f"))
        )))
      },

      test("Прерывание при ошибке парсинга") {
        for {
          parser <- ZIO.succeed(CsvParserImpl())
          result <- parser.parseLines(Seq("a,b", """c,"unclosed""", "e,f")).exit
        } yield assert(result)(fails(isSubtype[CsvError.ParseError](anything)))
      }
    )
  )
} 