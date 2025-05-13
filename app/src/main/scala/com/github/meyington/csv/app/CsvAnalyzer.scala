package com.github.meyington.csv.app

import com.github.meyington.csv.parser.*
import com.github.meyington.csv.parser.CsvParser.DefaultConfig
import zio.*
import zio.cli.*
import zio.cli.HelpDoc.Span.text
import java.nio.file.Path

/** Основное приложение для анализа CSV файлов.
  *
  * Функциональность:
  *   - Чтение CSV файлов
  *   - Базовая статистика (количество строк, колонок)
  *   - Фильтрация данных по колонкам
  *   - Вывод в различных форматах
  */
object CsvAnalyzer extends ZIOCliDefault:
  // Модель данных
  sealed trait CliCommand
  object CliCommand:
    case class Info(path: Path) extends CliCommand
    case class Head(path: Path, count: Int) extends CliCommand
    case class Filter(path: Path, column: String, value: String) extends CliCommand

  sealed trait CliError extends Exception
  object CliError:
    case class FileError(path: Path, msg: String) extends CliError:
      override def getMessage: String = s"Ошибка файла '$path': $msg"
    
    case class ValidationError(msg: String) extends CliError:
      override def getMessage: String = msg

  // Опции и аргументы
  private val pathArg = Args.file("input")
  
  private val countOpt = Options
    .integer("n")
    .alias("count")
    .withDefault(5)

  private val filterOpts = {
    val column = Options.text("column")
    val value = Options.text("value")
    (column ++ value)
  }

  // Справка
  private val infoHelp = HelpDoc.p("Показать информацию о CSV файле")
  private val headHelp = HelpDoc.p("Показать первые N строк файла")
  private val filterHelp = HelpDoc.p("Отфильтровать строки по значению в указанной колонке")
  private val pathHelp = HelpDoc.p("Путь к CSV файлу")
  private val countHelp = HelpDoc.p("Количество строк для вывода")
  private val columnHelp = HelpDoc.p("Название колонки для фильтрации")
  private val valueHelp = HelpDoc.p("Значение для фильтрации")

  // Команды
  private val infoCmd = Command("info", Options.none, pathArg)
    .map[CliCommand.Info](path => CliCommand.Info(path))
    .withHelp(infoHelp)

  private val headCmd = Command("head", countOpt, pathArg)
    .map[CliCommand.Head] { case (count: Int, path: Path) => CliCommand.Head(path, count) }
    .withHelp(headHelp)

  private val filterCmd = Command("filter", filterOpts, pathArg)
    .map[CliCommand.Filter] { case ((column: String, value: String), path: Path) => 
      CliCommand.Filter(path, column, value) 
    }
    .withHelp(filterHelp)

  private val commands = Command("csv-analyzer", Options.none, Args.none)
    .subcommands(infoCmd, headCmd, filterCmd)

  // Валидация
  private def validatePath(path: Path): IO[CliError, Path] = 
    for
      _ <- ZIO.unless(path.toFile.exists())(ZIO.fail(CliError.FileError(path, "файл не существует")))
      _ <- ZIO.unless(path.toFile.canRead())(ZIO.fail(CliError.FileError(path, "нет прав на чтение")))
    yield path

  private def validateCount(n: Int): IO[CliError, Int] =
    ZIO.unless(n > 0)(ZIO.fail(CliError.ValidationError("Количество строк должно быть положительным числом")))
      .as(n)

  private def validateFilterParams(column: String, value: String): IO[CliError, (String, String)] =
    for
      _ <- ZIO.unless(column.nonEmpty)(ZIO.fail(CliError.ValidationError("Название колонки не может быть пустым")))
      _ <- ZIO.unless(value.nonEmpty)(ZIO.fail(CliError.ValidationError("Значение для фильтрации не может быть пустым")))
    yield (column, value)

  // Бизнес-логика
  case class CsvProcessor(parser: CsvParser):
    def readLines(path: Path): IO[Throwable, Seq[String]] =
      validatePath(path)
        .flatMap(_ => ZIO.attempt(scala.io.Source.fromFile(path.toFile).getLines().toSeq))
        .tapError(e => ZIO.logError(s"Ошибка чтения файла: ${e.getMessage}"))

    def handleCsvError[A](zio: ZIO[Any, CsvError, A]): IO[Throwable, A] =
      zio.mapError(error => new Exception(s"Ошибка обработки CSV: ${error.toString}"))

    def showInfo(path: Path): IO[Throwable, Unit] =
      for
        lines <- readLines(path)
        rows <- handleCsvError(parser.parseLines(lines))
        _ <- Console.printLine(s"Количество строк: ${rows.size}")
        _ <- Console.printLine(s"Количество колонок: ${rows.headOption.map(_.values.size).getOrElse(0)}")
      yield ()

    def showHead(path: Path, count: Int): IO[Throwable, Unit] =
      for
        _ <- validateCount(count)
        lines <- readLines(path)
        rows <- handleCsvError(parser.parseLines(lines))
        _ <- ZIO.foreach(rows.take(count))(row => Console.printLine(row.values.mkString(",")))
      yield ()

    def filter(path: Path, column: String, value: String): IO[Throwable, Unit] =
      for
        _ <- validateFilterParams(column, value)
        lines <- readLines(path)
        rows <- handleCsvError(parser.parseLines(lines))
        headers = rows.headOption.map(_.values).getOrElse(Vector.empty)
        columnIndex = headers.indexOf(column)
        _ <- ZIO.when(columnIndex < 0)(
          ZIO.fail(new IllegalArgumentException(
            s"Колонка '$column' не найдена. Доступные колонки: ${headers.mkString(", ")}"
          ))
        )
        filtered = rows.tail.filter(_.values(columnIndex) == value)
        _ <- ZIO.foreach(filtered)(row => Console.printLine(row.values.mkString(",")))
      yield ()

  // Точка входа
  val cliApp = CliApp.make(
    name = "csv-analyzer",
    version = "0.2.0",
    summary = text("Инструмент для анализа CSV файлов"),
    command = commands
  ) { command =>
    val processor = CsvProcessor(CsvParserImpl(DefaultConfig))
    
    command match
      case CliCommand.Info(path) => processor.showInfo(path)
      case CliCommand.Head(path, count) => processor.showHead(path, count)
      case CliCommand.Filter(path, column, value) => processor.filter(path, column, value)
  } 