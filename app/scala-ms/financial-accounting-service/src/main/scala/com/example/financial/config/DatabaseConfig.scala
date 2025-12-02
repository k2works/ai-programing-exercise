package com.example.financial.config

import cats.effect.{IO, Resource}
import scalikejdbc.*
import scalikejdbc.config.DBs
import org.flywaydb.core.Flyway
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

/**
 * データベース設定
 */
object DatabaseConfig:

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  case class Config(
      url: String,
      user: String,
      password: String,
      driver: String = "org.postgresql.Driver"
  )

  /**
   * 環境変数から設定を読み込み
   */
  def loadFromEnv(): Config =
    Config(
      url = sys.env.getOrElse("DB_URL", "jdbc:postgresql://localhost:5432/financial_accounting"),
      user = sys.env.getOrElse("DB_USER", "postgres"),
      password = sys.env.getOrElse("DB_PASSWORD", "password")
    )

  /**
   * データベース接続を初期化（リソースとして管理）
   */
  def initialize(config: Config): Resource[IO, Unit] =
    Resource.make(
      IO {
        // JDBC ドライバーのロード
        Class.forName(config.driver)

        // ScalikeJDBC の設定
        ConnectionPool.singleton(
          config.url,
          config.user,
          config.password,
          ConnectionPoolSettings(
            initialSize = 5,
            maxSize = 20,
            connectionTimeoutMillis = 3000L,
            validationQuery = "SELECT 1"
          )
        )

        // グローバル設定
        GlobalSettings.loggingSQLAndTime = LoggingSQLAndTimeSettings(
          enabled = true,
          singleLineMode = true,
          logLevel = "DEBUG"
        )
      }
    )(_ =>
      IO {
        ConnectionPool.closeAll()
      }
    )

  /**
   * Flyway によるマイグレーション実行
   */
  def migrate(config: Config): IO[Int] =
    IO {
      val flyway = Flyway
        .configure()
        .dataSource(config.url, config.user, config.password)
        .locations("classpath:db/migration")
        .baselineOnMigrate(true)
        .load()

      val result = flyway.migrate()
      result.migrationsExecuted
    }
