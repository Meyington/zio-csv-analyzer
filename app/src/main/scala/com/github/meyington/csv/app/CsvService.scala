package com.github.meyington.csv.app

import zio.*
import com.github.meyington.csv.parser.*
import com.github.meyington.csv.parser.CsvParser.DefaultConfig
import java.nio.file.{Path, Paths}

trait CsvService:
  def showInfo(path: String): Task[CsvInfo]
  def showHead(path: String, count: Int): Task[List[String]]
  def filter(path: String, column: String, value: String): Task[List[String]]

case class CsvInfo(rowCount: Int, columnCount: Int)

object CsvService:
  def showInfo(path: String): ZIO[CsvService, Throwable, CsvInfo] =
    ZIO.serviceWithZIO[CsvService](_.showInfo(path))
    
  def showHead(path: String, count: Int): ZIO[CsvService, Throwable, List[String]] =
    ZIO.serviceWithZIO[CsvService](_.showHead(path, count))
    
  def filter(path: String, column: String, value: String): ZIO[CsvService, Throwable, List[String]] =
    ZIO.serviceWithZIO[CsvService](_.filter(path, column, value))

  val live: ULayer[CsvService] = ZLayer.succeed(
    new CsvService:
      private val parser = CsvParserImpl(DefaultConfig)
      
      private def readLines(path: String): Task[Seq[String]] =
        ZIO.attempt {
          val file = Paths.get(path).toFile
          if (!file.exists()) throw new IllegalArgumentException(s"Файл '$path' не существует")
          if (!file.canRead()) throw new IllegalArgumentException(s"Нет прав на чтение файла '$path'")
          scala.io.Source.fromFile(file).getLines().toSeq
        }

      def showInfo(path: String): Task[CsvInfo] =
        for
          lines <- readLines(path)
          rows <- ZIO.attempt(lines.size)
          cols <- ZIO.attempt(lines.headOption.map(_.split(",").length).getOrElse(0))
        yield CsvInfo(rows, cols)

      def showHead(path: String, count: Int): Task[List[String]] =
        for
          _ <- ZIO.when(count <= 0)(
            ZIO.fail(new IllegalArgumentException("Количество строк должно быть положительным"))
          )
          lines <- readLines(path)
        yield lines.take(count).toList

      def filter(path: String, column: String, value: String): Task[List[String]] =
        for
          lines <- readLines(path)
          headers: Array[String] = lines.headOption.map(_.split(",")).getOrElse(Array.empty[String])
          columnIndex = headers.indexOf(column)
          _ <- ZIO.when(columnIndex < 0)(
            ZIO.fail(new IllegalArgumentException(
              s"Колонка '$column' не найдена:( Доступные колонки: ${headers.mkString(", ")}"
            ))
          )
          filtered = lines.tail.filter(_.split(",")(columnIndex) == value)
        yield filtered.toList
  ) 