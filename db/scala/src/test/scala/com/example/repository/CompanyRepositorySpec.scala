package com.example.repository

import com.example.db.DatabaseSpec
import com.example.domain.{Company, CompanyGroup}
import scalikejdbc._

import java.time.LocalDateTime

class CompanyRepositorySpec extends DatabaseSpec {

  private def setupTestGroup(groupCode: String)(implicit session: DBSession): Unit = {
    val groupRepo = CompanyGroupRepository()
    val group = CompanyGroup(
      compGroupCode = groupCode,
      groupName = s"テストグループ${groupCode}",
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )
    groupRepo.create(group)
  }

  "CompanyRepository" should "取引先を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CompanyRepository()

    DB localTx { implicit session =>
      setupTestGroup("G101")

      val company = Company(
        compCode = "COMP0001",
        name = "株式会社テスト商事",
        kana = Some("カブシキガイシャテストショウジ"),
        supType = 1,
        zipCode = Some("100-0001"),
        state = Some("東京都"),
        address1 = Some("千代田区千代田1-1"),
        address2 = Some("テストビル1F"),
        noSalesFlg = 0,
        wideUseType = 0,
        compGroupCode = "G101",
        maxCredit = 10000000,
        tempCreditUp = 1000000,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      val result = repo.create(company)
      result shouldBe 1

      val retrieved = repo.findById("COMP0001")
      retrieved.isDefined shouldBe true
      retrieved.get.name shouldBe "株式会社テスト商事"
      retrieved.get.maxCredit shouldBe 10000000
    }
  }

  it should "全取引先を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CompanyRepository()

    DB localTx { implicit session =>
      setupTestGroup("G102")

      val company = Company(
        compCode = "COMP0002",
        name = "株式会社サンプル",
        compGroupCode = "G102",
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      repo.create(company)

      val companies = repo.findAll()
      companies should not be empty
      companies.map(_.compCode) should contain("COMP0002")
    }
  }

  it should "グループコードで取引先を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CompanyRepository()

    DB localTx { implicit session =>
      setupTestGroup("G103")

      val company1 = Company(
        compCode = "COMP0003",
        name = "ABC商事",
        compGroupCode = "G103",
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      val company2 = Company(
        compCode = "COMP0004",
        name = "XYZ物産",
        compGroupCode = "G103",
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      repo.create(company1)
      repo.create(company2)

      val companies = repo.findByGroup("G103")
      companies should have size 2
      companies.map(_.compCode) should contain allOf ("COMP0003", "COMP0004")
    }
  }

  it should "取引先を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CompanyRepository()

    DB localTx { implicit session =>
      setupTestGroup("G104")

      val company = Company(
        compCode = "COMP0005",
        name = "株式会社更新前",
        compGroupCode = "G104",
        maxCredit = 5000000,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      repo.create(company)

      val updated = company.copy(
        name = "株式会社更新後",
        maxCredit = 8000000,
        updateDate = LocalDateTime.now(),
        updater = "admin2"
      )

      val updateResult = repo.update(updated)
      updateResult shouldBe 1

      val retrieved = repo.findById("COMP0005")
      retrieved.isDefined shouldBe true
      retrieved.get.name shouldBe "株式会社更新後"
      retrieved.get.maxCredit shouldBe 8000000
      retrieved.get.updater shouldBe "admin2"
    }
  }

  it should "取引先を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CompanyRepository()

    DB localTx { implicit session =>
      setupTestGroup("G105")

      val company = Company(
        compCode = "COMP0006",
        name = "株式会社削除対象",
        compGroupCode = "G105",
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      repo.create(company)

      val deleteResult = repo.delete("COMP0006")
      deleteResult shouldBe 1

      val retrieved = repo.findById("COMP0006")
      retrieved shouldBe None
    }
  }
}
