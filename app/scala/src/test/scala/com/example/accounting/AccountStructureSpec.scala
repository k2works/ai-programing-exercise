package com.example.accounting

import com.example.db.DatabaseSpec
import com.example.accounting.domain.AccountStructure
import com.example.accounting.infrastructure.AccountStructureRepository
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*
import java.time.LocalDateTime

/**
 * 勘定科目構成マスタのテスト
 */
class AccountStructureSpec extends DatabaseSpec with BeforeAndAfterEach:

  val repository = new AccountStructureRepository()

  override def beforeEach(): Unit =
    super.beforeEach()

  behavior of "勘定科目構成マスタ"

  it should "勘定科目構成を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録（一意のプレフィックス）
      insertAccount("ST1A000", "資産の部", "資産")
      insertAccount("ST1A100", "流動資産", "資産")
      insertAccount("ST1A190", "現金及び預金", "資産")
      insertAccount("ST1A110", "現金", "資産")

      val structure = AccountStructure(
        accountCode = "ST1A110",
        tildePath = "ST1A000~ST1A100~ST1A190~ST1A110",
        hierarchyLevel = 4,
        parentAccountCode = Some("ST1A190"),
        createdAt = LocalDateTime.now(),
        updatedAt = LocalDateTime.now(),
      )

      val result = repository.insert(structure)
      result shouldBe 1

      val found = repository.findByCode("ST1A110")
      found should not be None
      found.get.tildePath shouldBe "ST1A000~ST1A100~ST1A190~ST1A110"
      found.get.hierarchyLevel shouldBe 4
    }
  }

  it should "すべての勘定科目構成を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertAccount("ST2A000", "資産の部", "資産")
      insertAccount("ST2A100", "流動資産", "資産")
      insertAccount("ST2A190", "現金及び預金", "資産")
      insertAccount("ST2A110", "現金", "資産")

      // 階層構造を登録
      insertStructure("ST2A000", "ST2A000", 1, None)
      insertStructure("ST2A100", "ST2A000~ST2A100", 2, Some("ST2A000"))
      insertStructure("ST2A190", "ST2A000~ST2A100~ST2A190", 3, Some("ST2A100"))
      insertStructure("ST2A110", "ST2A000~ST2A100~ST2A190~ST2A110", 4, Some("ST2A190"))

      val all = repository.findAll()
      all.filter(_.accountCode.startsWith("ST2")) should have size 4
    }
  }

  it should "親科目の子科目を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertAccount("ST3A000", "資産の部", "資産")
      insertAccount("ST3A100", "流動資産", "資産")
      insertAccount("ST3A190", "現金及び預金", "資産")
      insertAccount("ST3A110", "現金", "資産")
      insertAccount("ST3A120", "普通預金", "資産")

      insertStructure("ST3A000", "ST3A000", 1, None)
      insertStructure("ST3A100", "ST3A000~ST3A100", 2, Some("ST3A000"))
      insertStructure("ST3A190", "ST3A000~ST3A100~ST3A190", 3, Some("ST3A100"))
      insertStructure("ST3A110", "ST3A000~ST3A100~ST3A190~ST3A110", 4, Some("ST3A190"))
      insertStructure("ST3A120", "ST3A000~ST3A100~ST3A190~ST3A120", 4, Some("ST3A190"))

      val children = repository.findByParent("ST3A190")
      children should have size 2
      children.map(_.accountCode) should contain allOf ("ST3A110", "ST3A120")
    }
  }

  it should "チルダパスで子孫を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertAccount("ST4A000", "資産の部", "資産")
      insertAccount("ST4A100", "流動資産", "資産")
      insertAccount("ST4A190", "現金及び預金", "資産")
      insertAccount("ST4A110", "現金", "資産")
      insertAccount("ST4A120", "普通預金", "資産")

      insertStructure("ST4A000", "ST4A000", 1, None)
      insertStructure("ST4A100", "ST4A000~ST4A100", 2, Some("ST4A000"))
      insertStructure("ST4A190", "ST4A000~ST4A100~ST4A190", 3, Some("ST4A100"))
      insertStructure("ST4A110", "ST4A000~ST4A100~ST4A190~ST4A110", 4, Some("ST4A190"))
      insertStructure("ST4A120", "ST4A000~ST4A100~ST4A190~ST4A120", 4, Some("ST4A190"))

      // ST4A100の子孫をすべて取得
      val descendants = repository.findDescendants("ST4A000~ST4A100")
      descendants should have size 3 // ST4A190, ST4A110, ST4A120
    }
  }

  it should "階層レベルでフィルタリングできる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テスト用勘定科目を登録
      insertAccount("ST5A000", "資産の部", "資産")
      insertAccount("ST5A100", "流動資産", "資産")
      insertAccount("ST5A190", "現金及び預金", "資産")
      insertAccount("ST5A110", "現金", "資産")
      insertAccount("ST5A120", "普通預金", "資産")

      insertStructure("ST5A000", "ST5A000", 1, None)
      insertStructure("ST5A100", "ST5A000~ST5A100", 2, Some("ST5A000"))
      insertStructure("ST5A190", "ST5A000~ST5A100~ST5A190", 3, Some("ST5A100"))
      insertStructure("ST5A110", "ST5A000~ST5A100~ST5A190~ST5A110", 4, Some("ST5A190"))
      insertStructure("ST5A120", "ST5A000~ST5A100~ST5A190~ST5A120", 4, Some("ST5A190"))

      val level4 = repository.findByLevel(4)
      level4.filter(_.accountCode.startsWith("ST5")) should have size 2
    }
  }

  it should "勘定科目構成を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      insertAccount("ST6A000", "資産の部", "資産")
      insertAccount("ST6A100", "流動資産", "資産")
      insertAccount("ST6A190", "現金及び預金", "資産")
      insertAccount("ST6A110", "現金", "資産")

      insertStructure("ST6A110", "ST6A000~ST6A100~ST6A190~ST6A110", 4, Some("ST6A190"))

      val structure = repository.findByCode("ST6A110").get
      val updated = structure.copy(hierarchyLevel = 5)

      repository.update(updated)

      val found = repository.findByCode("ST6A110").get
      found.hierarchyLevel shouldBe 5
    }
  }

  it should "勘定科目構成を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      insertAccount("ST7A000", "資産の部", "資産")
      insertAccount("ST7A100", "流動資産", "資産")
      insertAccount("ST7A190", "現金及び預金", "資産")
      insertAccount("ST7A110", "現金", "資産")

      insertStructure("ST7A110", "ST7A000~ST7A100~ST7A190~ST7A110", 4, Some("ST7A190"))

      val deleted = repository.delete("ST7A110")
      deleted shouldBe 1

      val found = repository.findByCode("ST7A110")
      found shouldBe None
    }
  }

  // ヘルパーメソッド
  private def insertAccount(code: String, name: String, accountType: String)(implicit
      session: DBSession
  ): Unit =
    sql"""
      INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
      VALUES (${code}, ${name}, ${accountType}::account_type, ${BigDecimal("0")})
    """.update.apply()

  private def insertStructure(
      code: String,
      path: String,
      level: Int,
      parent: Option[String],
  )(implicit session: DBSession): Unit =
    sql"""
      INSERT INTO "勘定科目構成マスタ"
      ("勘定科目コード", "チルダパス", "階層レベル", "親科目コード")
      VALUES (${code}, ${path}, ${level}, ${parent})
    """.update.apply()
