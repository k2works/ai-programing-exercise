package com.example.db

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import scalikejdbc._

class FlywayMigrationSpec extends DatabaseSpec {

  "FlywayMigration" should "create flyway_schema_history table" in withContainers { container =>
    // マイグレーション実行
    val result = FlywayMigration.migrateCustom(
      url = container.jdbcUrl,
      user = container.username,
      password = container.password,
    )

    result.migrationsExecuted should be >= 0

    // 接続をセットアップ
    setupConnection(container)

    // flyway_schema_history テーブルが存在することを確認
    val tableExists = DB readOnly { implicit session =>
      sql"""
          SELECT COUNT(*)
          FROM information_schema.tables
          WHERE table_name = 'flyway_schema_history'
        """.map(rs => rs.int(1)).single.apply()
    }

    tableExists shouldBe Some(1)
  }

  it should "track migration versions" in withContainers { container =>
    // マイグレーション実行
    FlywayMigration.migrateCustom(
      url = container.jdbcUrl,
      user = container.username,
      password = container.password,
    )

    setupConnection(container)

    val migrationCount = DB readOnly { implicit session =>
      sql"SELECT COUNT(*) FROM flyway_schema_history"
        .map(rs => rs.int(1))
        .single
        .apply()
    }

    migrationCount.get should be >= 1
  }

  it should "be idempotent" in withContainers { container =>
    // 最初のマイグレーション実行
    val firstResult = FlywayMigration.migrateCustom(
      url = container.jdbcUrl,
      user = container.username,
      password = container.password,
    )

    setupConnection(container)

    val firstCount = DB readOnly { implicit session =>
      sql"SELECT COUNT(*) FROM flyway_schema_history"
        .map(rs => rs.int(1))
        .single
        .apply()
    }

    // 2回目のマイグレーション実行（冪等性の確認）
    val secondResult = FlywayMigration.migrateCustom(
      url = container.jdbcUrl,
      user = container.username,
      password = container.password,
    )

    val secondCount = DB readOnly { implicit session =>
      sql"SELECT COUNT(*) FROM flyway_schema_history"
        .map(rs => rs.int(1))
        .single
        .apply()
    }

    // 2回目は新しいマイグレーションが実行されないはず
    secondResult.migrationsExecuted shouldBe 0
    // マイグレーション履歴の数は変わらない
    secondCount shouldBe firstCount
  }

  it should "execute migrations in order" in withContainers { container =>
    FlywayMigration.migrateCustom(
      url = container.jdbcUrl,
      user = container.username,
      password = container.password,
    )

    setupConnection(container)

    val installedRanks = DB readOnly { implicit session =>
      sql"SELECT installed_rank FROM flyway_schema_history ORDER BY installed_rank"
        .map(rs => rs.int("installed_rank"))
        .list
        .apply()
    }

    // installed_rank が昇順であることを確認（マイグレーションが順番に実行されたことを確認）
    installedRanks shouldBe installedRanks.sorted

    // 最初のマイグレーションがバージョン1であることを確認
    val firstVersion = DB readOnly { implicit session =>
      sql"SELECT version FROM flyway_schema_history ORDER BY installed_rank LIMIT 1"
        .map(rs => rs.string("version"))
        .single
        .apply()
    }

    firstVersion shouldBe Some("1")
  }

  it should "validate migrations successfully" in withContainers { container =>
    FlywayMigration.migrateCustom(
      url = container.jdbcUrl,
      user = container.username,
      password = container.password,
    )

    // 検証は例外がスローされなければ成功
    noException should be thrownBy {
      val flyway = org.flywaydb.core.Flyway
        .configure()
        .dataSource(container.jdbcUrl, container.username, container.password)
        .locations("classpath:db/migration")
        .load()

      flyway.validate()
    }
  }
}
