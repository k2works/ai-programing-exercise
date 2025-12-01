ThisBuild / scalaVersion := "3.3.3"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "com.example"
ThisBuild / organizationName := "accounting-system"

lazy val scalikeJDBCVersion = "4.2.1"
lazy val flywayVersion = "10.4.1"
lazy val akkaHttpVersion = "10.5.3"
lazy val akkaVersion = "2.8.5"

lazy val root = (project in file("."))
  .settings(
    name := "accounting-system-db-scalikejdbc",
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
      "com.dimafeng" %% "testcontainers-scala-scalatest" % "0.43.0" % Test,
      "com.dimafeng" %% "testcontainers-scala-postgresql" % "0.43.0" % Test,

      // Akka HTTP
      "com.typesafe.akka" %% "akka-http" % akkaHttpVersion cross CrossVersion.for3Use2_13,
      "com.typesafe.akka" %% "akka-stream" % akkaVersion cross CrossVersion.for3Use2_13,
      "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion cross CrossVersion.for3Use2_13,
      "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion % Test cross CrossVersion.for3Use2_13,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test cross CrossVersion.for3Use2_13,

      // Configuration
      "com.typesafe" % "config" % "1.4.3",

      // Logging
      "ch.qos.logback" % "logback-classic" % "1.4.14",
      "com.typesafe.scala-logging" %% "scala-logging" % "3.9.5"
    ),
    // WartRemover settings
    wartremoverWarnings ++= Warts.unsafe,

    // scoverage settings
    coverageEnabled := false,
    coverageMinimumStmtTotal := 80,
    coverageFailOnMinimum := false,
    coverageHighlighting := true,
    coverageExcludedPackages := ".*db.migration.*",

    // Testcontainers settings - fork tests to pass environment variables
    Test / fork := true,
    Test / envVars := Map(
      "DOCKER_HOST" -> "tcp://localhost:2375",
      "TESTCONTAINERS_RYUK_DISABLED" -> "false"
    )
  )

// Custom tasks for database migrations
lazy val migrate = taskKey[Unit]("Run database migrations")
migrate := {
  (Compile / runMain).toTask(" com.example.db.MigrationRunner migrate").value
}

lazy val migrateInfo = taskKey[Unit]("Show migration information")
migrateInfo := {
  (Compile / runMain).toTask(" com.example.db.MigrationRunner info").value
}

lazy val migrateValidate = taskKey[Unit]("Validate migrations")
migrateValidate := {
  (Compile / runMain).toTask(" com.example.db.MigrationRunner validate").value
}
