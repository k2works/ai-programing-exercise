ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

lazy val root = (project in file("."))
  .settings(
    name := "fizzbuzz-scala",
    // WartRemover設定
    wartremoverWarnings ++= Warts.unsafe
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.18" % "test"

// カスタムタスク定義
lazy val format = taskKey[Unit]("Run scalafmt formatting")
format := {
  (Compile / scalafmt).value
  (Test / scalafmt).value
}

lazy val formatCheck = taskKey[Unit]("Check scalafmt formatting")
formatCheck := {
  (Compile / scalafmtCheck).value
  (Test / scalafmtCheck).value
}

lazy val lint = taskKey[Unit]("Run linting with WartRemover")
lint := {
  (Compile / compile).value
  (Test / compile).value
}

lazy val coverage = taskKey[Unit]("Run test coverage")
coverage := {
  (Test / test).value
  coverageReport.value
}

lazy val check = taskKey[Unit]("Run all checks: format, lint, test, coverage")
check := {
  formatCheck.value
  lint.value
  (Test / test).value
  coverageReport.value
}
