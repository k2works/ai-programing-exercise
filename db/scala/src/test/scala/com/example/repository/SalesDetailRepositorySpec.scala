package com.example.repository

import com.example.db.DatabaseSpec
import com.example.domain._
import scalikejdbc._

import java.time.LocalDateTime

class SalesDetailRepositorySpec extends DatabaseSpec {

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

    // 部門の作成
    val deptRepo = DepartmentRepository()
    val dept = Department(
      deptCode = s"1200$suffix",
      startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
      endDate = LocalDateTime.of(2999, 12, 31, 23, 59, 59),
      name = s"営業部$suffix",
      layer = 1,
      path = s"/1200$suffix/",
      lowestType = 1,
      slitYn = 1,
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    deptRepo.create(dept)

    // 取引先の作成
    val compRepo = CompanyRepository()
    val company = Company(
      compCode = s"CUS$suffix",
      name = s"株式会社顧客$suffix",
      kana = Some("カブシキガイシャコキャク"),
      supType = 1,
      zipCode = Some("100-0001"),
      state = Some("東京都"),
      address1 = Some("千代田区"),
      compGroupCode = s"G$suffix",
      maxCredit = 50000000,
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    compRepo.create(company)

    // 商品の作成
    val prodRepo = ProductRepository()
    val product = Product(
      prodCode = s"PROD$suffix",
      fullName = s"テスト商品正式名$suffix",
      name = s"テスト商品$suffix",
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    prodRepo.create(product)

    // 売上ヘッダーの作成
    val salesRepo = SalesRepository()
    val sales = Sales(
      salesNo = s"SL202401$suffix",
      salesDate = Some(LocalDateTime.of(2024, 1, 15, 14, 0)),
      salesType = 1,
      orderNo = None,
      deptCode = s"1200$suffix",
      startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
      compCode = s"CUS$suffix",
      salesAmnt = 100000,
      cmpTax = 10000,
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = Some("admin"),
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = Some("admin")
    )
    salesRepo.create(sales)
  }

  "SalesDetailRepository" should "売上明細を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val detailRepo = SalesDetailRepository()

    DB localTx { implicit session =>
      setupTestData("01")

      val detail = SalesDetail(
        salesNo = "SL20240101",
        rowNo = 1,
        prodCode = "PROD01",
        prodName = Some("テスト商品01"),
        unitPrice = 1000,
        deliveredQty = 10,
        qty = 10,
        discount = 0,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = Some("admin"),
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = Some("admin")
      )

      val result = detailRepo.create(detail)
      result shouldBe 1

      val retrieved = detailRepo.findBySalesNo("SL20240101")
      retrieved should not be empty
      retrieved.head.prodCode shouldBe "PROD01"
      retrieved.head.deliveredQty shouldBe 10
    }
  }

  it should "売上明細を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val detailRepo = SalesDetailRepository()

    DB localTx { implicit session =>
      setupTestData("02")

      val detail = SalesDetail(
        salesNo = "SL20240102",
        rowNo = 1,
        prodCode = "PROD02",
        prodName = Some("テスト商品02"),
        unitPrice = 1000,
        deliveredQty = 10,
        qty = 10,
        discount = 0,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = Some("admin"),
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = Some("admin")
      )

      detailRepo.create(detail)

      val updated = detail.copy(
        deliveredQty = 15,
        qty = 15,
        updateDate = LocalDateTime.now(),
        updater = Some("admin2")
      )

      val updateResult = detailRepo.update(updated)
      updateResult shouldBe 1

      val retrieved = detailRepo.findBySalesNo("SL20240102")
      retrieved should not be empty
      retrieved.head.deliveredQty shouldBe 15
      retrieved.head.qty shouldBe 15
    }
  }

  it should "売上明細を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val detailRepo = SalesDetailRepository()

    DB localTx { implicit session =>
      setupTestData("03")

      val detail = SalesDetail(
        salesNo = "SL20240103",
        rowNo = 1,
        prodCode = "PROD03",
        prodName = Some("テスト商品03"),
        unitPrice = 1000,
        deliveredQty = 10,
        qty = 10,
        discount = 0,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = Some("admin"),
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = Some("admin")
      )

      detailRepo.create(detail)

      val deleteResult = detailRepo.delete("SL20240103", 1)
      deleteResult shouldBe 1

      val retrieved = detailRepo.findBySalesNo("SL20240103")
      retrieved shouldBe empty
    }
  }
}
