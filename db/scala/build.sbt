ThisBuild / scalaVersion := "3.3.3"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "sales-management"

lazy val scalikeJDBCVersion = "4.2.1"
lazy val flywayVersion = "10.4.1"

lazy val root = (project in file("."))
  .settings(
    name := "sales-management-db-scalikejdbc",
    // テストの設定
    Test / parallelExecution := false,
    Test / fork := false,
    libraryDependencies ++= Seq(
      // Testing
      "org.scalatest" %% "scalatest" % "3.2.18" % Test,

      // Database - ScalikeJDBC
      "org.scalikejdbc" %% "scalikejdbc" % scalikeJDBCVersion,
      "org.scalikejdbc" %% "scalikejdbc-config" % scalikeJDBCVersion,
      "org.scalikejdbc" %% "scalikejdbc-test" % scalikeJDBCVersion % Test,

      // Database - PostgreSQL Driver
      "org.postgresql" % "postgresql" % "42.7.1",

      // Migration - Flyway
      "org.flywaydb" % "flyway-core" % flywayVersion,
      "org.flywaydb" % "flyway-database-postgresql" % flywayVersion,

      // Connection Pooling
      "com.zaxxer" % "HikariCP" % "5.1.0",

      // Testcontainers
      "com.dimafeng" %% "testcontainers-scala-scalatest" % "0.41.0" % Test,
      "com.dimafeng" %% "testcontainers-scala-postgresql" % "0.41.0" % Test,

      // Configuration
      "com.typesafe" % "config" % "1.4.3",

      // Logging
      "ch.qos.logback" % "logback-classic" % "1.4.14",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
    ),
    // scoverageの設定
    coverageEnabled := true,
    coverageMinimumStmtTotal := 80,
    coverageFailOnMinimum := false,
    coverageHighlighting := true,
    coverageExcludedPackages := ".*db.migration.*"
  )
  .enablePlugins(ScalafmtPlugin)

// ========================================
// カスタムタスクの定義
// ========================================

// フォーマットタスク
lazy val format = taskKey[Unit]("Format source code")
format := {
  (Compile / scalafmt).value
  (Test / scalafmt).value
}

// フォーマットチェックタスク
lazy val formatCheck = taskKey[Unit]("Check source code formatting")
formatCheck := {
  (Compile / scalafmtCheck).value
  (Test / scalafmtCheck).value
}

// 静的解析タスク
lazy val lint = taskKey[Unit]("Run static analysis")
lint := {
  (Compile / compile).value
}

// カバレッジタスク
lazy val coverage = taskKey[Unit]("Run tests with coverage")
coverage := Def.sequential(
  clean,
  Def.task { coverageEnabled := true },
  Test / test,
  coverageReport
).value

// 全品質チェックタスク
lazy val check = taskKey[Unit]("Run all quality checks")
check := Def.sequential(
  formatCheck,
  lint,
  Test / test
).value

// CI用の完全チェックタスク
lazy val ci = taskKey[Unit]("Run all checks for CI")
ci := Def.sequential(
  clean,
  formatCheck,
  lint,
  coverage
).value

// マイグレーションタスク
lazy val migrate = taskKey[Unit]("Run database migrations")
migrate := {
  (Compile / runMain).toTask(" com.example.db.MigrationRunner migrate").value
}

// マイグレーション情報タスク
lazy val migrateInfo = taskKey[Unit]("Show migration information")
migrateInfo := {
  (Compile / runMain).toTask(" com.example.db.MigrationRunner info").value
}

// マイグレーション検証タスク
lazy val migrateValidate = taskKey[Unit]("Validate migrations")
migrateValidate := {
  (Compile / runMain).toTask(" com.example.db.MigrationRunner validate").value
}
