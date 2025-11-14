package com.example.repository

import com.example.db.DatabaseSpec
import com.example.domain.ProductCategory
import scalikejdbc._

import java.time.LocalDateTime

class ProductCategoryRepositorySpec extends DatabaseSpec {

  "ProductCategoryRepository" should "商品分類を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = ProductCategoryRepository()

    val category = ProductCategory(
      categoryCode = "CAT101",
      name = "PC機器",
      layer = 1,
      path = "/PC/",
      lowestType = 0,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )

    val result = DB localTx { implicit session =>
      repo.create(category)
    }

    result shouldBe 1

    val retrieved = DB readOnly { implicit session =>
      repo.findById("CAT101")
    }

    retrieved.isDefined shouldBe true
    retrieved.get.name shouldBe "PC機器"
    retrieved.get.layer shouldBe 1
  }

  it should "全商品分類を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = ProductCategoryRepository()

    val category = ProductCategory(
      categoryCode = "CAT102",
      name = "サーバー機器",
      layer = 1,
      path = "/SERVER/",
      lowestType = 0,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )

    DB localTx { implicit session =>
      repo.create(category)
    }

    val categories = DB readOnly { implicit session =>
      repo.findAll()
    }

    categories should not be empty
    categories.map(_.categoryCode) should contain("CAT102")
  }

  it should "商品分類を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = ProductCategoryRepository()

    val category = ProductCategory(
      categoryCode = "CAT103",
      name = "ネットワーク機器",
      layer = 1,
      path = "/NETWORK/",
      lowestType = 0,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )

    DB localTx { implicit session =>
      repo.create(category)
    }

    val updated = category.copy(
      name = "ネットワーク機器類",
      updateDate = LocalDateTime.now(),
      updater = "admin2"
    )

    val updateResult = DB localTx { implicit session =>
      repo.update(updated)
    }

    updateResult shouldBe 1

    val retrieved = DB readOnly { implicit session =>
      repo.findById("CAT103")
    }

    retrieved.isDefined shouldBe true
    retrieved.get.name shouldBe "ネットワーク機器類"
    retrieved.get.updater shouldBe "admin2"
  }

  it should "商品分類を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = ProductCategoryRepository()

    val category = ProductCategory(
      categoryCode = "CAT104",
      name = "周辺機器",
      layer = 1,
      path = "/PERIPHERAL/",
      lowestType = 0,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )

    DB localTx { implicit session =>
      repo.create(category)
    }

    val deleteResult = DB localTx { implicit session =>
      repo.delete("CAT104")
    }

    deleteResult shouldBe 1

    val retrieved = DB readOnly { implicit session =>
      repo.findById("CAT104")
    }

    retrieved shouldBe None
  }

  it should "パスプレフィックスで商品分類を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = ProductCategoryRepository()

    val category1 = ProductCategory(
      categoryCode = "CAT105",
      name = "テスト機器",
      layer = 1,
      path = "/TEST/",
      lowestType = 0,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )

    val category2 = ProductCategory(
      categoryCode = "CAT106",
      name = "テストノート型",
      layer = 2,
      path = "/TEST/NOTE/",
      lowestType = 1,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )

    DB localTx { implicit session =>
      repo.create(category1)
      repo.create(category2)
    }

    val results = DB readOnly { implicit session =>
      repo.findByPathPrefix("/TEST/")
    }

    results should have size 2
    results.map(_.categoryCode) should contain allOf ("CAT105", "CAT106")
  }
}
