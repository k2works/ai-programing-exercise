package com.example.repository

import com.example.db.DatabaseSpec
import com.example.domain.Product
import scalikejdbc._

import java.time.LocalDateTime

class ProductRepositorySpec extends DatabaseSpec {

  "ProductRepository" should "商品を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = ProductRepository()

    val product = Product(
      prodCode = "PROD001",
      fullName = "ノートパソコン Core i7",
      name = "ノートPC",
      kana = Some("ノートピーシー"),
      prodType = Some("1"),
      serialNo = Some("NB-2024-001"),
      unitPrice = 150000,
      poPrice = 120000,
      primeCost = 100000,
      taxType = 1,
      categoryCode = Some("CAT001"),
      wideUseType = Some(0),
      stockManageType = 1,
      stockReserveType = Some(1),
      supCode = Some("SUP001"),
      supSubNo = Some(1),
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )

    val result = DB localTx { implicit session =>
      repo.create(product)
    }

    result shouldBe 1

    val retrieved = DB readOnly { implicit session =>
      repo.findById("PROD001")
    }

    retrieved.isDefined shouldBe true
    retrieved.get.fullName shouldBe "ノートパソコン Core i7"
    retrieved.get.unitPrice shouldBe 150000
  }

  it should "全商品を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = ProductRepository()

    val product = Product(
      prodCode = "PROD002",
      fullName = "デスクトップパソコン Core i9",
      name = "デスクトップ",
      kana = Some("デスクトップピーシー"),
      prodType = Some("1"),
      unitPrice = 200000,
      poPrice = 160000,
      primeCost = 140000,
      taxType = 1,
      stockManageType = 1,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )

    DB localTx { implicit session =>
      repo.create(product)
    }

    val products = DB readOnly { implicit session =>
      repo.findAll()
    }

    products should not be empty
    products.map(_.prodCode) should contain("PROD002")
  }

  it should "商品を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = ProductRepository()

    val product = Product(
      prodCode = "PROD003",
      fullName = "タブレット端末",
      name = "タブレット",
      kana = Some("タブレット"),
      prodType = Some("1"),
      unitPrice = 80000,
      poPrice = 60000,
      primeCost = 50000,
      taxType = 1,
      stockManageType = 1,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )

    DB localTx { implicit session =>
      repo.create(product)
    }

    val updated = product.copy(
      unitPrice = 75000,
      updateDate = LocalDateTime.now(),
      updater = "admin2"
    )

    val updateResult = DB localTx { implicit session =>
      repo.update(updated)
    }

    updateResult shouldBe 1

    val retrieved = DB readOnly { implicit session =>
      repo.findById("PROD003")
    }

    retrieved.isDefined shouldBe true
    retrieved.get.unitPrice shouldBe 75000
    retrieved.get.updater shouldBe "admin2"
  }

  it should "商品を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = ProductRepository()

    val product = Product(
      prodCode = "PROD004",
      fullName = "スマートフォン",
      name = "スマホ",
      kana = Some("スマートフォン"),
      prodType = Some("1"),
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

    DB localTx { implicit session =>
      repo.create(product)
    }

    val deleteResult = DB localTx { implicit session =>
      repo.delete("PROD004")
    }

    deleteResult shouldBe 1

    val retrieved = DB readOnly { implicit session =>
      repo.findById("PROD004")
    }

    retrieved shouldBe None
  }

  it should "商品分類で商品を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = ProductRepository()

    val product1 = Product(
      prodCode = "PROD005",
      fullName = "ノートパソコン",
      name = "ノートPC",
      categoryCode = Some("CAT002"),
      unitPrice = 150000,
      poPrice = 120000,
      primeCost = 100000,
      taxType = 1,
      stockManageType = 1,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )

    val product2 = Product(
      prodCode = "PROD006",
      fullName = "デスクトップパソコン",
      name = "デスクトップ",
      categoryCode = Some("CAT002"),
      unitPrice = 200000,
      poPrice = 160000,
      primeCost = 140000,
      taxType = 1,
      stockManageType = 1,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )

    DB localTx { implicit session =>
      repo.create(product1)
      repo.create(product2)
    }

    val results = DB readOnly { implicit session =>
      repo.findByCategory("CAT002")
    }

    results should have size 2
    results.map(_.prodCode) should contain allOf ("PROD005", "PROD006")
  }
}
