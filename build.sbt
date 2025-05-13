ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.6"
ThisBuild / organization := "com.example"

lazy val commonSettings = Seq(
  scalaVersion := "3.3.6",
  scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
)

lazy val root = (project in file("."))
  .aggregate(core, app)
  .settings(
    name := "zio-csv-analyzer",
    publish / skip := true
  )

lazy val core = (project in file("core"))
  .settings(commonSettings)
  .settings(
    name := "core",
    libraryDependencies += "dev.zio" %% "zio" % "2.1.17"
  )

lazy val app = (project in file("app"))
  .dependsOn(core)
  .settings(commonSettings)
  .settings(
    name := "app",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.1.17",
      "dev.zio" %% "zio-streams" % "2.1.17",
      "dev.zio" %% "zio-cli" % "0.7.1",
      "dev.zio" %% "zio-test" % "2.1.17" % Test,
      "dev.zio" %% "zio-test-sbt" % "2.1.17" % Test
    ),
    testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
  )

libraryDependencies ++= Seq(
  "dev.zio" %% "zio-test"       % "2.1.17" % Test,
  "dev.zio" %% "zio-test-sbt"   % "2.1.17" % Test
)


