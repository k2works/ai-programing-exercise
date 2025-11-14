package com.example.db

import scalikejdbc._
import com.typesafe.config.ConfigFactory

/**
 * データベース接続の設定と初期化を行うオブジェクト
 */
object DatabaseConfig {

  /**
   * デフォルトのデータベース接続を初期化
   */
  def setup(): Unit = {
    val config = ConfigFactory.load()

    // デフォルト接続の設定
    val defaultConfig = config.getConfig("db.default")

    val settings = ConnectionPoolSettings(
      initialSize = defaultConfig.getInt("poolInitialSize"),
      maxSize = defaultConfig.getInt("poolMaxSize"),
      connectionTimeoutMillis = defaultConfig.getLong("poolConnectionTimeoutMillis"),
      validationQuery = defaultConfig.getString("poolValidationQuery"),
    )

    ConnectionPool.singleton(
      url = defaultConfig.getString("url"),
      user = defaultConfig.getString("user"),
      password = defaultConfig.getString("password"),
      settings = settings,
    )
  }

  /**
   * テスト用のデータベース接続を初期化
   */
  def setupTest(): Unit = {
    val config = ConfigFactory.load()

    // テスト接続の設定
    val testConfig = config.getConfig("db.test")

    val settings = ConnectionPoolSettings(
      initialSize = testConfig.getInt("poolInitialSize"),
      maxSize = testConfig.getInt("poolMaxSize"),
      connectionTimeoutMillis = testConfig.getLong("poolConnectionTimeoutMillis"),
      validationQuery = testConfig.getString("poolValidationQuery"),
    )

    ConnectionPool.singleton(
      url = testConfig.getString("url"),
      user = testConfig.getString("user"),
      password = testConfig.getString("password"),
      settings = settings,
    )
  }

  /**
   * カスタム設定でデータベース接続を初期化
   */
  def setupCustom(
    url: String,
    user: String,
    password: String,
    driver: String = "org.postgresql.Driver",
    poolSettings: ConnectionPoolSettings = ConnectionPoolSettings(),
  ): Unit = {
    Class.forName(driver)
    ConnectionPool.singleton(url, user, password, poolSettings)
  }

  /**
   * 接続プールをクローズ
   */
  def close(): Unit =
    ConnectionPool.closeAll()
}
