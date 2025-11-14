package com.example.db

import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.scalatest.TestContainerForAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.BeforeAndAfterAll
import org.testcontainers.utility.DockerImageName
import scalikejdbc._

/**
 * データベーステストの基底クラス
 *
 * testcontainersを使用してPostgreSQLコンテナを管理し、
 * テストごとに独立したデータベース環境を提供します。
 */
trait DatabaseSpec
    extends AnyFlatSpec
    with Matchers
    with TestContainerForAll
    with BeforeAndAfterAll {

  // PostgreSQL 15-alpineイメージを使用
  override val containerDef: PostgreSQLContainer.Def =
    PostgreSQLContainer.Def(
      dockerImageName = DockerImageName.parse("postgres:15-alpine"),
      databaseName = "testdb",
      username = "test",
      password = "test",
    )

  /**
   * testcontainersから接続をセットアップ
   */
  def setupConnection(container: PostgreSQLContainer): Unit =
    DatabaseConfig.setupCustom(
      url = container.jdbcUrl,
      user = container.username,
      password = container.password,
      driver = container.driverClassName,
      poolSettings = ConnectionPoolSettings(
        initialSize = 1,
        maxSize = 5,
      ),
    )

  /**
   * testcontainersでマイグレーションを実行
   */
  def runMigrations(container: PostgreSQLContainer): Unit =
    FlywayMigration.migrateCustom(
      url = container.jdbcUrl,
      user = container.username,
      password = container.password,
    )

  /**
   * testcontainersでマイグレーションと接続セットアップを実行
   */
  def setupWithMigrations(container: PostgreSQLContainer): Unit = {
    runMigrations(container)
    setupConnection(container)
  }

  /**
   * 全テスト終了後にコネクションプールをクローズ
   */
  override def afterAll(): Unit = {
    ConnectionPool.closeAll()
    super.afterAll()
  }

}
