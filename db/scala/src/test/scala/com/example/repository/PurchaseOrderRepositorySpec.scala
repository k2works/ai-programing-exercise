package com.example.repository

import com.example.db.DatabaseSpec
import com.example.domain.{Company, CompanyGroup, Department, Employee, Product, PurchaseOrder, PurchaseOrderDetail, Supplier}
import scalikejdbc._

import java.time.LocalDateTime

class PurchaseOrderRepositorySpec extends DatabaseSpec {

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
  }

  "PurchaseOrderRepository" should "発注を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val poRepo = PurchaseOrderRepository()
    val detailRepo = PurchaseOrderDetailRepository()

    DB localTx { implicit session =>
      setupTestData()

      val po = PurchaseOrder(
        poNo = "PO20240115001",
        poDate = LocalDateTime.of(2024, 1, 15, 10, 0),
        deptCode = "110000",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
        supCode = "SUP001",
        supSubNo = 0,
        empCode = "E001",
        completeFlg = 0,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      val result = poRepo.create(po)
      result shouldBe 1

      val detail = PurchaseOrderDetail(
        poNo = "PO20240115001",
        poDetailNo = 1,
        prodCode = "PROD001",
        qty = 100,
        price = 1000,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      detailRepo.create(detail) shouldBe 1

      val retrieved = poRepo.findByNo("PO20240115001")
      retrieved.isDefined shouldBe true
      retrieved.get.supCode shouldBe "SUP001"

      val details = detailRepo.findByPONo("PO20240115001")
      details should not be empty
      details.head.qty shouldBe 100
    }
  }

  it should "発注番号で検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val poRepo = PurchaseOrderRepository()

    DB localTx { implicit session =>
      setupTestData()

      val po = PurchaseOrder(
        poNo = "PO20240115002",
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

      val retrieved = poRepo.findByNo("PO20240115002")
      retrieved.isDefined shouldBe true
      retrieved.get.poNo shouldBe "PO20240115002"

      val notFound = poRepo.findByNo("PO99999999999")
      notFound shouldBe None
    }
  }

  it should "仕入先別に発注を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val poRepo = PurchaseOrderRepository()

    DB localTx { implicit session =>
      setupTestData()

      val po1 = PurchaseOrder(
        poNo = "PO20240115003",
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

      val po2 = PurchaseOrder(
        poNo = "PO20240115004",
        poDate = LocalDateTime.of(2024, 1, 16, 10, 0),
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

      poRepo.create(po1)
      poRepo.create(po2)

      val pos = poRepo.findBySupplier("SUP001", 0)
      pos should have size 2
      pos.map(_.poNo) should contain allOf ("PO20240115003", "PO20240115004")
    }
  }

  it should "発注を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val poRepo = PurchaseOrderRepository()

    DB localTx { implicit session =>
      setupTestData()

      val po = PurchaseOrder(
        poNo = "PO20240115005",
        poDate = LocalDateTime.of(2024, 1, 15, 10, 0),
        deptCode = "110000",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
        supCode = "SUP001",
        supSubNo = 0,
        empCode = "E001",
        completeFlg = 0,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      poRepo.create(po)

      val updated = po.copy(
        completeFlg = 1,
        updateDate = LocalDateTime.now(),
        updater = "admin2"
      )

      val updateResult = poRepo.update(updated)
      updateResult shouldBe 1

      val retrieved = poRepo.findByNo("PO20240115005")
      retrieved.isDefined shouldBe true
      retrieved.get.completeFlg shouldBe 1
      retrieved.get.updater shouldBe "admin2"
    }
  }

  it should "発注を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val poRepo = PurchaseOrderRepository()

    DB localTx { implicit session =>
      setupTestData()

      val po = PurchaseOrder(
        poNo = "PO20240115006",
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

      val deleteResult = poRepo.delete("PO20240115006")
      deleteResult shouldBe 1

      val retrieved = poRepo.findByNo("PO20240115006")
      retrieved shouldBe None
    }
  }
}
