package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.domain.{PriceByCustomer, Product}
import scalikejdbc._

import java.time.LocalDateTime

class PriceByCustomerRepositorySpec extends DatabaseSpec {

  private def setupTestProduct(prodCode: String)(implicit session: DBSession): Unit = {
    val productRepo = ProductRepository()
    val product = Product(
      prodCode = prodCode,
      fullName = s"テスト商品${prodCode}",
      name = s"商品${prodCode}",
      unitPrice = 100000,
      poPrice = 80000,
      primeCost = 70000,
      taxType = 1,
      stockManageType = 1,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )
    productRepo.create(product)
  }

  "PriceByCustomerRepository" should "顧客別単価を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = PriceByCustomerRepository()

    DB localTx { implicit session =>
      setupTestProduct("PROD201")

      val price = PriceByCustomer(
        prodCode = "PROD201",
        compCode = "CUST001",
        unitPrice = 95000,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      val result = repo.create(price)
      result shouldBe 1

      val retrieved = repo.findById("PROD201", "CUST001")
      retrieved.isDefined shouldBe true
      retrieved.get.unitPrice shouldBe 95000
    }
  }

  it should "商品コードで顧客別単価を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = PriceByCustomerRepository()

    DB localTx { implicit session =>
      setupTestProduct("PROD202")

      val price1 = PriceByCustomer(
        prodCode = "PROD202",
        compCode = "CUST002",
        unitPrice = 92000,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      val price2 = PriceByCustomer(
        prodCode = "PROD202",
        compCode = "CUST003",
        unitPrice = 93000,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      repo.create(price1)
      repo.create(price2)

      val prices = repo.findByProduct("PROD202")
      prices should have size 2
      prices.map(_.compCode) should contain allOf ("CUST002", "CUST003")
    }
  }

  it should "取引先コードで顧客別単価を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = PriceByCustomerRepository()

    DB localTx { implicit session =>
      setupTestProduct("PROD203")
      setupTestProduct("PROD204")

      val price1 = PriceByCustomer(
        prodCode = "PROD203",
        compCode = "CUST004",
        unitPrice = 90000,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      val price2 = PriceByCustomer(
        prodCode = "PROD204",
        compCode = "CUST004",
        unitPrice = 91000,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      repo.create(price1)
      repo.create(price2)

      val prices = repo.findByCustomer("CUST004")
      prices should have size 2
      prices.map(_.prodCode) should contain allOf ("PROD203", "PROD204")
    }
  }

  it should "顧客別単価を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = PriceByCustomerRepository()

    DB localTx { implicit session =>
      setupTestProduct("PROD205")

      val price = PriceByCustomer(
        prodCode = "PROD205",
        compCode = "CUST005",
        unitPrice = 88000,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      repo.create(price)

      val updated = price.copy(
        unitPrice = 85000,
        updateDate = LocalDateTime.now(),
        updater = "admin2"
      )

      val updateResult = repo.update(updated)
      updateResult shouldBe 1

      val retrieved = repo.findById("PROD205", "CUST005")
      retrieved.isDefined shouldBe true
      retrieved.get.unitPrice shouldBe 85000
      retrieved.get.updater shouldBe "admin2"
    }
  }

  it should "顧客別単価を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = PriceByCustomerRepository()

    DB localTx { implicit session =>
      setupTestProduct("PROD206")

      val price = PriceByCustomer(
        prodCode = "PROD206",
        compCode = "CUST006",
        unitPrice = 87000,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin"
      )

      repo.create(price)

      val deleteResult = repo.delete("PROD206", "CUST006")
      deleteResult shouldBe 1

      val retrieved = repo.findById("PROD206", "CUST006")
      retrieved shouldBe None
    }
  }
}
