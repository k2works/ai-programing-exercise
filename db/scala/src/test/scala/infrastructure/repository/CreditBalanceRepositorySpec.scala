package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.domain._
import scalikejdbc._

import java.time.LocalDateTime

class CreditBalanceRepositorySpec extends DatabaseSpec {

  private def setupTestData(suffix: String)(implicit session: DBSession): Unit = {
    // 取引先グループの作成
    val groupRepo = CompanyGroupRepository()
    val group = CompanyGroup(
      compGroupCode = s"G$suffix",
      groupName = s"テストグループ$suffix",
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    groupRepo.create(group)

    // 取引先の作成
    val compRepo = CompanyRepository()
    val company = Company(
      compCode = s"COMP$suffix",
      name = s"株式会社テスト$suffix",
      kana = Some("カブシキガイシャテスト"),
      supType = 0,
      zipCode = Some("100-0001"),
      state = Some("東京都"),
      address1 = Some("千代田区"),
      compGroupCode = s"G$suffix",
      maxCredit = 10000000,
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    compRepo.create(company)
  }

  "CreditBalanceRepository" should "与信残高を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val balanceRepo = CreditBalanceRepository()

    DB localTx { implicit session =>
      setupTestData("001")

      val balance = CreditBalance(
        compCode = "COMP001",
        orderBalance = 1000000,
        recBalance = 500000,
        payBalance = 200000,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = Some("admin"),
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = Some("admin")
      )

      val result = balanceRepo.create(balance)
      result shouldBe 1

      val retrieved = balanceRepo.findByCompCode("COMP001")
      retrieved.isDefined shouldBe true
      retrieved.get.orderBalance shouldBe 1000000
      retrieved.get.recBalance shouldBe 500000
      retrieved.get.payBalance shouldBe 200000
    }
  }

  it should "与信残高を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val balanceRepo = CreditBalanceRepository()

    DB localTx { implicit session =>
      setupTestData("002")

      val balance = CreditBalance(
        compCode = "COMP002",
        orderBalance = 1000000,
        recBalance = 500000,
        payBalance = 200000,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = Some("admin"),
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = Some("admin")
      )

      balanceRepo.create(balance)

      val updated = balance.copy(
        orderBalance = 1500000,
        recBalance = 800000,
        updateDate = LocalDateTime.now(),
        updater = Some("admin2")
      )

      val updateResult = balanceRepo.update(updated)
      updateResult shouldBe 1

      val retrieved = balanceRepo.findByCompCode("COMP002")
      retrieved.isDefined shouldBe true
      retrieved.get.orderBalance shouldBe 1500000
      retrieved.get.recBalance shouldBe 800000
    }
  }

  it should "与信残高を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val balanceRepo = CreditBalanceRepository()

    DB localTx { implicit session =>
      setupTestData("003")

      val balance = CreditBalance(
        compCode = "COMP003",
        orderBalance = 1000000,
        recBalance = 500000,
        payBalance = 200000,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = Some("admin"),
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = Some("admin")
      )

      balanceRepo.create(balance)

      val deleteResult = balanceRepo.delete("COMP003")
      deleteResult shouldBe 1

      val retrieved = balanceRepo.findByCompCode("COMP003")
      retrieved shouldBe None
    }
  }

  it should "全ての与信残高を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val balanceRepo = CreditBalanceRepository()

    DB localTx { implicit session =>
      setupTestData("004")
      setupTestData("005")

      val balance1 = CreditBalance(
        compCode = "COMP004",
        orderBalance = 1000000,
        recBalance = 500000,
        payBalance = 200000,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = Some("admin"),
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = Some("admin")
      )

      val balance2 = CreditBalance(
        compCode = "COMP005",
        orderBalance = 2000000,
        recBalance = 1000000,
        payBalance = 300000,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = Some("admin"),
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = Some("admin")
      )

      balanceRepo.create(balance1)
      balanceRepo.create(balance2)

      val allBalances = balanceRepo.findAll()
      allBalances.length should be >= 2
      allBalances.exists(_.compCode == "COMP004") shouldBe true
      allBalances.exists(_.compCode == "COMP005") shouldBe true
    }
  }
}
