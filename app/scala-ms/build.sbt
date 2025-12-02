// Root build.sbt for Scala Microservices

ThisBuild / scalaVersion := "3.3.4"
ThisBuild / version := "1.0.0"
ThisBuild / organization := "com.example"

// Common settings for all projects
lazy val commonSettings = Seq(
  scalacOptions ++= Seq(
    "-deprecation",
    "-feature",
    "-unchecked",
    "-Xfatal-warnings"
  ),
  libraryDependencies ++= Seq(
    // Logging
    "ch.qos.logback" % "logback-classic" % "1.4.14",
    "org.typelevel" %% "log4cats-slf4j" % "2.6.0",

    // Test
    "org.scalatest" %% "scalatest" % "3.2.17" % Test,
    "org.scalatestplus" %% "scalacheck-1-17" % "3.2.17.0" % Test
  )
)

// Http4s common dependencies
lazy val http4sDeps = Seq(
  "org.http4s" %% "http4s-ember-server" % "0.23.27",
  "org.http4s" %% "http4s-ember-client" % "0.23.27",
  "org.http4s" %% "http4s-dsl" % "0.23.27",
  "org.http4s" %% "http4s-circe" % "0.23.27"
)

// Tapir dependencies
lazy val tapirDeps = Seq(
  "com.softwaremill.sttp.tapir" %% "tapir-core" % "1.10.0",
  "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % "1.10.0",
  "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % "1.10.0",
  "com.softwaremill.sttp.tapir" %% "tapir-swagger-ui-bundle" % "1.10.0"
)

// Circe dependencies
lazy val circeDeps = Seq(
  "io.circe" %% "circe-generic" % "0.14.7",
  "io.circe" %% "circe-parser" % "0.14.7"
)

// Database dependencies
lazy val dbDeps = Seq(
  "org.scalikejdbc" %% "scalikejdbc" % "4.3.0",
  "org.scalikejdbc" %% "scalikejdbc-config" % "4.3.0",
  "org.postgresql" % "postgresql" % "42.7.3",
  "org.flywaydb" % "flyway-core" % "10.10.0",
  "org.flywaydb" % "flyway-database-postgresql" % "10.10.0",
  "com.zaxxer" % "HikariCP" % "5.1.0"
)

// Test container dependencies
lazy val testContainerDeps = Seq(
  "com.dimafeng" %% "testcontainers-scala-scalatest" % "0.41.3" % Test,
  "com.dimafeng" %% "testcontainers-scala-postgresql" % "0.41.3" % Test
)

// Root project
lazy val root = (project in file("."))
  .aggregate(
    common,
    financialAccountingService,
    managementAccountingService,
    apiGateway
  )
  .settings(
    name := "scala-ms",
    publish / skip := true
  )

// Common shared library
lazy val common = (project in file("common"))
  .settings(
    name := "common",
    commonSettings,
    libraryDependencies ++= circeDeps ++ Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4"
    )
  )

// Financial Accounting Service
lazy val financialAccountingService = (project in file("financial-accounting-service"))
  .dependsOn(common)
  .enablePlugins(JavaAppPackaging, DockerPlugin)
  .settings(
    name := "financial-accounting-service",
    commonSettings,
    libraryDependencies ++= http4sDeps ++ tapirDeps ++ circeDeps ++ dbDeps ++ testContainerDeps ++ Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4"
    ),
    Docker / packageName := "financial-accounting-service",
    Docker / version := version.value,
    dockerBaseImage := "eclipse-temurin:21-jre",
    dockerExposedPorts := Seq(8080),
    dockerUpdateLatest := true
  )

// Management Accounting Service
lazy val managementAccountingService = (project in file("management-accounting-service"))
  .dependsOn(common)
  .enablePlugins(JavaAppPackaging, DockerPlugin)
  .settings(
    name := "management-accounting-service",
    commonSettings,
    libraryDependencies ++= http4sDeps ++ tapirDeps ++ circeDeps ++ dbDeps ++ testContainerDeps ++ Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4"
    ),
    Docker / packageName := "management-accounting-service",
    Docker / version := version.value,
    dockerBaseImage := "eclipse-temurin:21-jre",
    dockerExposedPorts := Seq(8081),
    dockerUpdateLatest := true
  )

// API Gateway
lazy val apiGateway = (project in file("api-gateway"))
  .dependsOn(common)
  .enablePlugins(JavaAppPackaging, DockerPlugin)
  .settings(
    name := "api-gateway",
    commonSettings,
    libraryDependencies ++= http4sDeps ++ circeDeps ++ Seq(
      "org.typelevel" %% "cats-effect" % "3.5.4"
    ),
    Docker / packageName := "api-gateway",
    Docker / version := version.value,
    dockerBaseImage := "eclipse-temurin:21-jre",
    dockerExposedPorts := Seq(8000),
    dockerUpdateLatest := true
  )
