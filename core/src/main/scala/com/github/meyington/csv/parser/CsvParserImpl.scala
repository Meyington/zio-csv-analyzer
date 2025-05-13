package com.github.meyington.csv.parser

import zio.*

/** Реализация парсера CSV с поддержкой экранирования и кавычек.
  *
  * Особенности реализации:
  *   - Поддержка настраиваемого разделителя полей
  *   - Поддержка значений в кавычках
  *   - Поддержка экранирования специальных символов
  *   - Корректная обработка пустых значений
  */
class CsvParserImpl(config: CsvConfig) extends CsvParser {
  import CsvError.*

  /** Состояние парсера для отслеживания текущей позиции и режима */
  private case class ParserState(
    currentField: StringBuilder = new StringBuilder,
    fields: Vector[String] = Vector.empty,
    inQuotes: Boolean = false,
    escaped: Boolean = false
  )

  def parseLine(line: String): IO[CsvError, CsvRow] = {
    if (line == null) {
      ZIO.fail(ParseError("Null input"))
    } else {
      ZIO.attempt {
        var state = ParserState()
        
        def handleChar(c: Char): Unit = {
          if (state.escaped) {
            // Обработка экранированного символа
            state.currentField.append(c)
            state = state.copy(escaped = false)
          } else if (c == config.escapeChar) {
            // Начало экранирования
            state = state.copy(escaped = true)
          } else if (c == config.quoteChar) {
            // Переключение режима кавычек
            state = state.copy(inQuotes = !state.inQuotes)
          } else if (c == config.delimiter && !state.inQuotes) {
            // Конец поля (если не в кавычках)
            state = state.copy(
              fields = state.fields :+ state.currentField.toString,
              currentField = new StringBuilder
            )
          } else {
            // Обычный символ
            state.currentField.append(c)
          }
        }

        // Обработка всех символов
        line.foreach(handleChar)

        // Добавление последнего поля
        val finalFields = state.fields :+ state.currentField.toString

        // Проверка корректности парсинга
        if (state.inQuotes) {
          throw new Exception("Unclosed quotes")
        }
        if (state.escaped) {
          throw new Exception("Incomplete escape sequence")
        }

        CsvRow(finalFields)
      }.mapError(e => ParseError(e.getMessage))
    }
  }

  def parseLines(lines: Seq[String]): IO[CsvError, Vector[CsvRow]] = {
    if (lines == null) {
      ZIO.fail(ParseError("Null input"))
    } else {
      ZIO.foreach(lines.toVector)(parseLine)
    }
  }
}

object CsvParserImpl {
  /** Создает парсер с настройками по умолчанию */
  def apply(): CsvParserImpl = new CsvParserImpl(CsvParser.DefaultConfig)

  /** Создает парсер с пользовательскими настройками */
  def apply(config: CsvConfig): CsvParserImpl = new CsvParserImpl(config)
} 