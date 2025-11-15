package com.example.repository

import com.example.db.DatabaseSpec
import com.example.domain._
import scalikejdbc._

import java.time.LocalDateTime

class PurchaseRepositorySpec extends DatabaseSpec {

  private def setupTestData()(implicit session: DBSession): Unit = {
    // 取引先グループの作成
    val groupRepo = CompanyGroupRepository()
    val group = CompanyGroup(
      compGroupCode = "G001",
      groupName = "テストグループ",
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    groupRepo.create(group)

    // 部門の作成
    val deptRepo = DepartmentRepository()
    val dept = Department(
      deptCode = "110000",
      startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
      endDate = LocalDateTime.of(2999, 12, 31, 23, 59, 59),
      name = "購買部",
      layer = 1,
      path = "/110000/",
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
      empCode = "E001",
      name = "山田太郎",
      kana = "ヤマダタロウ",
      loginPassword = "password",
      tel = "03-1111-1111",
      fax = "03-1111-1112",
      deptCode = "110000",
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
      compCode = "SUP001",
      name = "ABC商事株式会社",
      kana = Some("エービーシーショウジ"),
      supType = 0,
      zipCode = Some("100-0001"),
      state = Some("東京都"),
      address1 = Some("千代田区"),
      compGroupCode = "G001",
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
      supCode = "SUP001",
      supSubNo = 0,
      supType = 0,
      name = Some("ABC商事株式会社"),
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    supRepo.create(supplier)

    // 倉庫の作成
    val whRepo = WarehouseRepository()
    val warehouse = Warehouse(
      whCode = "WH1",
      name = "テスト倉庫",
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    whRepo.create(warehouse)

    // 商品の作成
    val prodRepo = ProductRepository()
    val product = Product(
      prodCode = "PROD001",
      fullName = "テスト商品正式名",
      name = "テスト商品",
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    prodRepo.create(product)

    // 発注の作成
    val poRepo = PurchaseOrderRepository()
    val po = PurchaseOrder(
      poNo = "PO20240115001",
      poDate = LocalDateTime.of(2024, 1, 15, 10, 0),
      deptCode = "110000",
      startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
      supCode = "SUP001",
      supSubNo = 0,
      empCode = "E001",
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
      setupTestData()

      val purchase = Purchase(
        purchaseNo = "PUR2024011501",
        purchaseDate = LocalDateTime.of(2024, 1, 15, 14, 0),
        poNo = "PO20240115001",
        whCode = "WH1",
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
        prodCode = "PROD001",
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
      retrieved.get.poNo shouldBe "PO20240115001"

      val details = detailRepo.findByPurchaseNo("PUR2024011501")
      details should not be empty
      details.head.rotNo shouldBe "LOT20240115001"
    }
  }

  it should "発注番号で仕入を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val purRepo = PurchaseRepository()

    DB localTx { implicit session =>
      setupTestData()

      val purchase = Purchase(
        purchaseNo = "PUR2024011502",
        purchaseDate = LocalDateTime.of(2024, 1, 15, 14, 0),
        poNo = "PO20240115001",
        whCode = "WH1",
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      purRepo.create(purchase)

      val purchases = purRepo.findByPONo("PO20240115001")
      purchases should not be empty
      purchases.head.purchaseNo shouldBe "PUR2024011502"
    }
  }

  it should "仕入を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val purRepo = PurchaseRepository()

    DB localTx { implicit session =>
      setupTestData()

      val purchase = Purchase(
        purchaseNo = "PUR2024011503",
        purchaseDate = LocalDateTime.of(2024, 1, 15, 14, 0),
        poNo = "PO20240115001",
        whCode = "WH1",
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
      setupTestData()

      val purchase = Purchase(
        purchaseNo = "PUR2024011504",
        purchaseDate = LocalDateTime.of(2024, 1, 15, 14, 0),
        poNo = "PO20240115001",
        whCode = "WH1",
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
