package com.github.meyington.csv.app

import zio.*
import com.github.meyington.csv.parser.*
import com.github.meyington.csv.parser.CsvParser.DefaultConfig
import java.nio.file.{Path, Paths}

object ConsoleApp extends ZIOAppDefault:
  private val processor = CsvAnalyzer.CsvProcessor(CsvParserImpl(DefaultConfig))

  private def showMenu: ZIO[Any, Nothing, Unit] =
    Console.printLine(
      """
      |CSV Анализатор - Главное меню
      |------------------------
      |1. Показать информацию о файле
      |2. Показать первые N строк
      |3. Фильтровать по значению в колонке
      |0. Выход
      |
      |Выберите действие (0-3):""".stripMargin
    ).orDie

  private def getFilePath: ZIO[Any, Throwable, Path] =
    for
      _ <- Console.printLine("Введите путь к CSV файлу:").orDie
      path <- Console.readLine.map(Paths.get(_))
    yield path

  private def getLineCount: ZIO[Any, Throwable, Int] =
    for
      _ <- Console.printLine("Сколько строк показать?").orDie
      count <- Console.readLine.flatMap(str => 
        ZIO.attempt(str.toInt).mapError(_ => 
          new IllegalArgumentException("Пожалуйста, введите целое число")
        )
      )
      _ <- ZIO.when(count <= 0)(
        ZIO.fail(new IllegalArgumentException("Количество строк должно быть положительным"))
      )
    yield count

  private def getFilterParams: ZIO[Any, Throwable, (String, String)] =
    for
      _ <- Console.printLine("Введите название колонки:").orDie
      column <- Console.readLine
      _ <- Console.printLine("Введите искомое значение:").orDie
      value <- Console.readLine
    yield (column, value)

  private def handleCommand(cmd: String): ZIO[Any, Throwable, Boolean] =
    cmd match
      case "1" => // Информация о файле
        for
          path <- getFilePath
          _ <- processor.showInfo(path)
        yield true

      case "2" => // Первые N строк
        for
          path <- getFilePath
          count <- getLineCount
          _ <- processor.showHead(path, count)
        yield true

      case "3" => // Фильтрация
        for
          path <- getFilePath
          params <- getFilterParams
          _ <- processor.filter(path, params._1, params._2)
        yield true

      case "0" => // Выход
        ZIO.succeed(false)

      case _ =>
        Console.printLine("Неверный выбор. Пожалуйста, выберите 0-3").as(true)

  def mainLoop: ZIO[Any, Throwable, Unit] =
    for
      _ <- showMenu
      input <- Console.readLine
      continue <- handleCommand(input).catchAll { error =>
        Console.printLine(s"Ошибка: ${error.getMessage}").as(true)
      }
      _ <- ZIO.when(continue)(mainLoop)
    yield ()

  def run = mainLoop.catchAll { error =>
    Console.printLine(s"Критическая ошибка: ${error.getMessage}").orDie
  } 