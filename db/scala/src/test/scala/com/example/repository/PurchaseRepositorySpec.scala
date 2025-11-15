package com.example.repository

import com.example.db.DatabaseSpec
import com.example.domain._
import scalikejdbc._

import java.time.LocalDateTime

class PurchaseRepositorySpec extends DatabaseSpec {

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
      deptCode = s"1100$suffix",
      startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
      endDate = LocalDateTime.of(2999, 12, 31, 23, 59, 59),
      name = s"購買部$suffix",
      layer = 1,
      path = s"/1100$suffix/",
      lowestType = 1,
      slitYn = 1,
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    deptRepo.create(dept)

    // 社員の作成
    val empRepo = EmployeeRepository()
    val emp = Employee(
      empCode = s"E$suffix",
      name = "山田太郎",
      kana = "ヤマダタロウ",
      loginPassword = "password",
      tel = "03-1111-1111",
      fax = "03-1111-1112",
      deptCode = s"1100$suffix",
      startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
      occuCode = "01",
      approvalCode = "01",
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    empRepo.create(emp)

    // 取引先の作成
    val compRepo = CompanyRepository()
    val company = Company(
      compCode = s"SUP$suffix",
      name = s"ABC商事株式会社$suffix",
      kana = Some("エービーシーショウジ"),
      supType = 0,
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

    // 仕入先の作成
    val supRepo = SupplierRepository()
    val supplier = Supplier(
      supCode = s"SUP$suffix",
      supSubNo = 0,
      supType = 0,
      name = Some(s"ABC商事株式会社$suffix"),
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    supRepo.create(supplier)

    // 倉庫の作成
    val whRepo = WarehouseRepository()
    val warehouse = Warehouse(
      whCode = s"WH$suffix",
      name = s"テスト倉庫$suffix",
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    whRepo.create(warehouse)

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

    // 発注の作成
    val poRepo = PurchaseOrderRepository()
    val po = PurchaseOrder(
      poNo = s"PO2024011500$suffix",
      poDate = LocalDateTime.of(2024, 1, 15, 10, 0),
      deptCode = s"1100$suffix",
      startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
      supCode = s"SUP$suffix",
      supSubNo = 0,
      empCode = s"E$suffix",
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    poRepo.create(po)
  }

  "PurchaseRepository" should "仕入を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val purRepo = PurchaseRepository()
    val detailRepo = PurchaseDetailRepository()

    DB localTx { implicit session =>
      setupTestData("01")

      val purchase = Purchase(
        purchaseNo = "PUR2024011501",
        purchaseDate = LocalDateTime.of(2024, 1, 15, 14, 0),
        poNo = "PO202401150001",
        whCode = "WH01",
        completeFlg = 0,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      val result = purRepo.create(purchase)
      result shouldBe 1

      val detail = PurchaseDetail(
        purchaseNo = "PUR2024011501",
        purchaseDetailNo = 1,
        prodCode = "PROD01",
        rotNo = "LOT20240115001",
        qty = 100,
        price = 1000,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      detailRepo.create(detail) shouldBe 1

      val retrieved = purRepo.findByNo("PUR2024011501")
      retrieved.isDefined shouldBe true
      retrieved.get.poNo shouldBe "PO202401150001"

      val details = detailRepo.findByPurchaseNo("PUR2024011501")
      details should not be empty
      details.head.rotNo shouldBe "LOT20240115001"
    }
  }

  it should "発注番号で仕入を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val purRepo = PurchaseRepository()

    DB localTx { implicit session =>
      setupTestData("02")

      val purchase = Purchase(
        purchaseNo = "PUR2024011502",
        purchaseDate = LocalDateTime.of(2024, 1, 15, 14, 0),
        poNo = "PO202401150002",
        whCode = "WH02",
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      purRepo.create(purchase)

      val purchases = purRepo.findByPONo("PO202401150002")
      purchases should not be empty
      purchases.head.purchaseNo shouldBe "PUR2024011502"
    }
  }

  it should "仕入を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val purRepo = PurchaseRepository()

    DB localTx { implicit session =>
      setupTestData("03")

      val purchase = Purchase(
        purchaseNo = "PUR2024011503",
        purchaseDate = LocalDateTime.of(2024, 1, 15, 14, 0),
        poNo = "PO202401150003",
        whCode = "WH03",
        completeFlg = 0,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      purRepo.create(purchase)

      val updated = purchase.copy(
        completeFlg = 1,
        updateDate = LocalDateTime.now(),
        updater = "admin2"
      )

      val updateResult = purRepo.update(updated)
      updateResult shouldBe 1

      val retrieved = purRepo.findByNo("PUR2024011503")
      retrieved.isDefined shouldBe true
      retrieved.get.completeFlg shouldBe 1
      retrieved.get.updater shouldBe "admin2"
    }
  }

  it should "仕入を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val purRepo = PurchaseRepository()

    DB localTx { implicit session =>
      setupTestData("04")

      val purchase = Purchase(
        purchaseNo = "PUR2024011504",
        purchaseDate = LocalDateTime.of(2024, 1, 15, 14, 0),
        poNo = "PO202401150004",
        whCode = "WH04",
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      purRepo.create(purchase)

      val deleteResult = purRepo.delete("PUR2024011504")
      deleteResult shouldBe 1

      val retrieved = purRepo.findByNo("PUR2024011504")
      retrieved shouldBe None
    }
  }
}
