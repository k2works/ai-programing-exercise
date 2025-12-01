package com.example.db

import com.dimafeng.testcontainers.PostgreSQLContainer
import com.dimafeng.testcontainers.scalatest.TestContainerForAll
import org.scalatest.BeforeAndAfterAll
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.testcontainers.utility.DockerImageName
import scalikejdbc.*

class DatabaseConfigSpec
    extends AnyFlatSpec
    with Matchers
    with TestContainerForAll
    with BeforeAndAfterAll:

  // PostgreSQL 15のコンテナイメージを指定
  override val containerDef: PostgreSQLContainer.Def =
    PostgreSQLContainer.Def(
      dockerImageName = DockerImageName.parse("postgres:15-alpine"),
      databaseName = "testdb",
      username = "test",
      password = "test",
    )

  // 各テスト後にコネクションプールをクローズ
  override def afterAll(): Unit =
    super.afterAll()
    ConnectionPool.closeAll()

  "DatabaseConfig" should "load configuration from application.conf" in {
    val config = com.typesafe.config.ConfigFactory.load()

    val defaultConfig = config.getConfig("db.default")
    defaultConfig.getString("driver") shouldBe "org.postgresql.Driver"
    defaultConfig.getString("url") should include("postgresql")
    defaultConfig.getString("user") shouldBe "postgres"
    defaultConfig.getInt("poolMaxSize") should be > 0
  }

  it should "create a valid database connection with testcontainers" in withContainers {
    container =>
      // testcontainersから接続情報を取得してセットアップ
      DatabaseConfig.setupCustom(
        url = container.jdbcUrl,
        user = container.username,
        password = container.password,
        driver = container.driverClassName,
      )

      // 簡単な接続テスト
      val result = DB readOnly { implicit session =>
        sql"SELECT 1".map(rs => rs.int(1)).single.apply()
      }

      result shouldBe Some(1)

      // クリーンアップ
      ConnectionPool.closeAll()
  }

  it should "execute a simple query" in withContainers { container =>
    DatabaseConfig.setupCustom(
      url = container.jdbcUrl,
      user = container.username,
      password = container.password,
      driver = container.driverClassName,
    )

    val result = DB readOnly { implicit session =>
      sql"SELECT CURRENT_TIMESTAMP".map(rs => rs.timestamp(1)).single.apply()
    }

    result should not be None

    ConnectionPool.closeAll()
  }

  it should "handle multiple concurrent connections" in withContainers { container =>
    DatabaseConfig.setupCustom(
      url = container.jdbcUrl,
      user = container.username,
      password = container.password,
      driver = container.driverClassName,
      poolSettings = ConnectionPoolSettings(
        initialSize = 5,
        maxSize = 10,
      ),
    )

    // 複数のクエリを実行
    val results = (1 to 5).map { i =>
      DB readOnly { implicit session => sql"SELECT $i".map(rs => rs.int(1)).single.apply() }
    }

    results shouldBe List(Some(1), Some(2), Some(3), Some(4), Some(5))

    ConnectionPool.closeAll()
  }

  it should "support read-only transactions" in withContainers { container =>
    DatabaseConfig.setupCustom(
      url = container.jdbcUrl,
      user = container.username,
      password = container.password,
      driver = container.driverClassName,
    )

    // readOnly トランザクションの確認
    val result = DB readOnly { implicit session =>
      sql"SELECT 1 AS value".map(rs => rs.int("value")).single.apply()
    }

    result shouldBe Some(1)

    ConnectionPool.closeAll()
  }

  it should "support local transactions" in withContainers { container =>
    DatabaseConfig.setupCustom(
      url = container.jdbcUrl,
      user = container.username,
      password = container.password,
      driver = container.driverClassName,
    )

    // 一時テーブルを作成してテスト
    DB localTx { implicit session =>
      sql"""
        CREATE TEMP TABLE test_table (
          id SERIAL PRIMARY KEY,
          value INTEGER
        )
      """.execute.apply()

      sql"INSERT INTO test_table (value) VALUES (100)".update.apply()
    }

    // データが挿入されたことを確認
    val result = DB readOnly { implicit session =>
      sql"SELECT value FROM test_table".map(rs => rs.int("value")).single.apply()
    }

    result shouldBe Some(100)

    ConnectionPool.closeAll()
  }
