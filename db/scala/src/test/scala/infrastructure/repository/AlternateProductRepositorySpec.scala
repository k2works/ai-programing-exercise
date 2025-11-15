package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.entity.{AlternateProduct, Product}
import scalikejdbc._

import java.time.LocalDateTime

class AlternateProductRepositorySpec extends DatabaseSpec {

  private def setupTestProduct(prodCode: String)(implicit session: DBSession): Unit = {
    val productRepo = ProductRepository()
    val product     = Product(
      prodCode = prodCode,
      fullName = s"テスト商品$prodCode",
      name = s"商品$prodCode",
      unitPrice = 100000,
      poPrice = 80000,
      primeCost = 70000,
      taxType = 1,
      stockManageType = 1,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin",
    )
    productRepo.create(product)
  }

  "AlternateProductRepository" should "代替商品を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = AlternateProductRepository()

    DB localTx { implicit session =>
      setupTestProduct("PROD301")
      setupTestProduct("PROD302")

      val alternate = AlternateProduct(
        prodCode = "PROD301",
        altProdCode = "PROD302",
        priority = 1,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      val result = repo.create(alternate)
      result shouldBe 1

      val retrieved = repo.findById("PROD301", "PROD302")
      retrieved.isDefined shouldBe true
      retrieved.get.priority shouldBe 1
    }
  }

  it should "商品コードで代替商品を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = AlternateProductRepository()

    DB localTx { implicit session =>
      setupTestProduct("PROD303")
      setupTestProduct("PROD304")
      setupTestProduct("PROD305")

      val alternate1 = AlternateProduct(
        prodCode = "PROD303",
        altProdCode = "PROD304",
        priority = 1,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      val alternate2 = AlternateProduct(
        prodCode = "PROD303",
        altProdCode = "PROD305",
        priority = 2,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(alternate1)
      repo.create(alternate2)

      val alternates = repo.findByProduct("PROD303")
      alternates should have size 2
      alternates.map(_.altProdCode) should contain allOf ("PROD304", "PROD305")
      alternates.head.altProdCode shouldBe "PROD304" // 優先順位でソート
    }
  }

  it should "代替商品を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = AlternateProductRepository()

    DB localTx { implicit session =>
      setupTestProduct("PROD306")
      setupTestProduct("PROD307")

      val alternate = AlternateProduct(
        prodCode = "PROD306",
        altProdCode = "PROD307",
        priority = 1,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(alternate)

      val updated = alternate.copy(
        priority = 2,
        updateDate = LocalDateTime.now(),
        updater = "admin2",
      )

      val updateResult = repo.update(updated)
      updateResult shouldBe 1

      val retrieved = repo.findById("PROD306", "PROD307")
      retrieved.isDefined shouldBe true
      retrieved.get.priority shouldBe 2
      retrieved.get.updater shouldBe "admin2"
    }
  }

  it should "代替商品を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = AlternateProductRepository()

    DB localTx { implicit session =>
      setupTestProduct("PROD308")
      setupTestProduct("PROD309")

      val alternate = AlternateProduct(
        prodCode = "PROD308",
        altProdCode = "PROD309",
        priority = 1,
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(alternate)

      val deleteResult = repo.delete("PROD308", "PROD309")
      deleteResult shouldBe 1

      val retrieved = repo.findById("PROD308", "PROD309")
      retrieved shouldBe None
    }
  }
}
