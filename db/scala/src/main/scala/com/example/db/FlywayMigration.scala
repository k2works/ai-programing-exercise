package com.example.db

import com.typesafe.config.ConfigFactory
import org.flywaydb.core.Flyway
import org.flywaydb.core.api.output.MigrateResult

/**
 * Flywayを使用したデータベースマイグレーション管理
 */
object FlywayMigration {

  /**
   * デフォルト設定でマイグレーションを実行
   */
  def migrate(): MigrateResult = {
    val config       = ConfigFactory.load()
    val dbConfig     = config.getConfig("db.default")
    val flywayConfig = config.getConfig("flyway.default")

    val flyway = Flyway
      .configure()
      .dataSource(
        dbConfig.getString("url"),
        dbConfig.getString("user"),
        dbConfig.getString("password"),
      )
      .locations(flywayConfig.getString("locations"))
      .cleanDisabled(flywayConfig.getBoolean("cleanDisabled"))
      .load()

    flyway.migrate()
  }

  /**
   * テスト用データベースでマイグレーションを実行
   */
  def migrateTest(): MigrateResult = {
    val config       = ConfigFactory.load()
    val dbConfig     = config.getConfig("db.test")
    val flywayConfig = config.getConfig("flyway.test")

    val flyway = Flyway
      .configure()
      .dataSource(
        dbConfig.getString("url"),
        dbConfig.getString("user"),
        dbConfig.getString("password"),
      )
      .locations(flywayConfig.getString("locations"))
      .cleanDisabled(flywayConfig.getBoolean("cleanDisabled"))
      .load()

    flyway.migrate()
  }

  /**
   * カスタム接続情報でマイグレーションを実行
   */
  def migrateCustom(
    url: String,
    user: String,
    password: String,
    locations: String = "classpath:db/migration",
  ): MigrateResult = {
    val flyway = Flyway
      .configure()
      .dataSource(url, user, password)
      .locations(locations)
      .cleanDisabled(false) // カスタム接続では制限を緩和
      .load()

    flyway.migrate()
  }

  /**
   * データベースをクリーン（全テーブル削除）
   * テスト環境でのみ使用すること！
   */
  def clean(url: String, user: String, password: String): Unit = {
    val flyway = Flyway
      .configure()
      .dataSource(url, user, password)
      .cleanDisabled(false)
      .load()

    flyway.clean()
  }

  /**
   * マイグレーション情報を表示
   */
  def info(url: String, user: String, password: String): Unit = {
    val flyway = Flyway
      .configure()
      .dataSource(url, user, password)
      .locations("classpath:db/migration")
      .load()

    val info = flyway.info()
    println("Flyway Migration Information:")
    println("=" * 80)
    info.all().foreach { migration =>
      println(
        s"Version: ${migration.getVersion}, " +
          s"Description: ${migration.getDescription}, " +
          s"State: ${migration.getState}"
      )
    }
    println("=" * 80)
  }

  /**
   * マイグレーションの検証
   */
  def validate(): Unit = {
    val config   = ConfigFactory.load()
    val dbConfig = config.getConfig("db.default")

    val flyway = Flyway
      .configure()
      .dataSource(
        dbConfig.getString("url"),
        dbConfig.getString("user"),
        dbConfig.getString("password"),
      )
      .locations("classpath:db/migration")
      .load()

    flyway.validate()
    println("✓ Migration validation successful")
  }

}

/**
 * コマンドラインから実行可能なマイグレーションランナー
 */
object MigrationRunner {

  def main(args: Array[String]): Unit =
    args.headOption match {
      case Some("migrate") =>
        val result = FlywayMigration.migrate()
        println(
          s"✓ Migration completed: ${result.migrationsExecuted} migrations executed"
        )

      case Some("validate") =>
        FlywayMigration.validate()

      case Some("info") =>
        val config   = ConfigFactory.load()
        val dbConfig = config.getConfig("db.default")
        FlywayMigration.info(
          dbConfig.getString("url"),
          dbConfig.getString("user"),
          dbConfig.getString("password"),
        )

      case _ =>
        println("""
          |Usage: sbt "runMain com.example.db.MigrationRunner <command>"
          |
          |Commands:
          |  migrate   - Run pending migrations
          |  validate  - Validate applied migrations
          |  info      - Display migration information
          |""".stripMargin)
    }

}
