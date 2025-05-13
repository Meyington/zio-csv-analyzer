package com.github.meyington.csv.app

import zio.*
import zio.test.*
import zio.test.Assertion.*
import java.nio.file.Paths
import zio.test.TestAspect.*

object CsvAnalyzerSpec extends ZIOSpecDefault:
  private val testDataPath = "app/src/test/resources/data/mtcars.csv"
  private val nonExistentPath = "non-existent.csv"

  private def simulateUserInput(inputs: String*): ZIO[Any, Nothing, Unit] =
    TestConsole.feedLines(inputs*)

  private def runMenuAndGetOutput(inputs: String*): ZIO[Any, Throwable, String] =
    for
      fiber <- ConsoleApp.mainLoop.fork
      _ <- TestConsole.feedLines(inputs*)
      _ <- fiber.await
      output <- TestConsole.output
    yield output.mkString("\n")

  def spec = suite("CSV Анализатор")(
    suite("Информация о файле")(
      test("Показывает информацию о существующем файле") {
        for
          info <- CsvService.showInfo(testDataPath)
        yield assert(info)(
          hasField("rowCount", (info: CsvInfo) => info.rowCount, equalTo(33)) && 
          hasField("columnCount", (info: CsvInfo) => info.columnCount, equalTo(12))
        )
      },

      test("Сообщает об ошибке для несуществующего файла") {
        for
          result <- CsvService.showInfo(nonExistentPath).exit
        yield assert(result)(fails(hasMessage(containsString("не существует"))))
      }
    ),

    suite("Просмотр начала файла")(
      test("Показывает указанное количество строк") {
        for
          lines <- CsvService.showHead(testDataPath, 3)
        yield assert(lines)(
          hasSize(equalTo(3)) &&
          contains("\"model\",\"mpg\",\"cyl\",\"disp\",\"hp\",\"drat\",\"wt\",\"qsec\",\"vs\",\"am\",\"gear\",\"carb\"") &&
          contains("\"Mazda RX4\",21,6,160,110,3.9,2.62,16.46,0,1,4,4") &&
          contains("\"Mazda RX4 Wag\",21,6,160,110,3.9,2.875,17.02,0,1,4,4")
        )
      },

      test("Проверяет положительное количество строк") {
        for
          result <- CsvService.showHead(testDataPath, -1).exit
        yield assert(result)(fails(hasMessage(containsString("должно быть положительным"))))
      }
    ),

    suite("Фильтрация данных")(
      test("Фильтрует по точному совпадению") {
        for
          lines <- CsvService.filter(testDataPath, "\"model\"", "\"Mazda RX4\"")
        yield assert(lines)(
          hasSize(equalTo(1)) &&
          exists(containsString("\"Mazda RX4\"")) &&
          not(exists(containsString("\"Datsun\"")))
        )
      },

      test("Обрабатывает несуществующую колонку") {
        for
          result <- CsvService.filter(testDataPath, "несуществующая", "значение").exit
        yield assert(result)(fails(hasMessage(containsString("Колонка 'несуществующая' не найдена"))))
      },

      test("Возвращает пустой список при отсутствии совпадений") {
        for
          lines <- CsvService.filter(testDataPath, "\"model\"", "\"несуществующая_модель\"")
        yield assert(lines)(isEmpty)
      }
    )
  ).provideLayer(CsvService.live) @@ sequential 