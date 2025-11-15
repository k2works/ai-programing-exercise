package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.entity.{
  Company,
  CompanyGroup,
  Department,
  Employee,
  Product,
  PurchaseOrder,
  PurchaseOrderDetail,
  Supplier,
}
import scalikejdbc._

import java.time.LocalDateTime

class PurchaseOrderRepositorySpec extends DatabaseSpec {

  private def setupTestData(suffix: String)(implicit session: DBSession): Unit = {
    // 取引先グループの作成
    val groupRepo = CompanyGroupRepository()
    val group     = CompanyGroup(
      compGroupCode = s"G$suffix",
      groupName = s"テストグループ$suffix",
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin",
    )
    groupRepo.create(group)

    // 部門の作成
    val deptRepo = DepartmentRepository()
    val dept     = Department(
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
      updater = "admin",
    )
    deptRepo.create(dept)

    // 社員の作成
    val empRepo = EmployeeRepository()
    val emp     = Employee(
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
      updater = "admin",
    )
    empRepo.create(emp)

    // 取引先の作成
    val compRepo = CompanyRepository()
    val company  = Company(
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
      updater = "admin",
    )
    compRepo.create(company)

    // 仕入先の作成
    val supRepo  = SupplierRepository()
    val supplier = Supplier(
      supCode = s"SUP$suffix",
      supSubNo = 0,
      supType = 0,
      name = Some(s"ABC商事株式会社$suffix"),
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin",
    )
    supRepo.create(supplier)

    // 商品の作成
    val prodRepo = ProductRepository()
    val product  = Product(
      prodCode = s"PROD$suffix",
      fullName = s"テスト商品正式名$suffix",
      name = s"テスト商品$suffix",
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin",
    )
    prodRepo.create(product)
  }

  "PurchaseOrderRepository" should "発注を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val poRepo     = PurchaseOrderRepository()
    val detailRepo = PurchaseOrderDetailRepository()

    DB localTx { implicit session =>
      setupTestData("01")

      val po = PurchaseOrder(
        poNo = "PO20240115001",
        poDate = LocalDateTime.of(2024, 1, 15, 10, 0),
        deptCode = "110001",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
        supCode = "SUP01",
        supSubNo = 0,
        empCode = "E01",
        completeFlg = 0,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin",
      )

      val result = poRepo.create(po)
      result shouldBe 1

      val detail = PurchaseOrderDetail(
        poNo = "PO20240115001",
        poDetailNo = 1,
        prodCode = "PROD01",
        qty = 100,
        price = 1000,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin",
      )

      detailRepo.create(detail) shouldBe 1

      val retrieved = poRepo.findByNo("PO20240115001")
      retrieved.isDefined shouldBe true
      retrieved.get.supCode shouldBe "SUP01"

      val details = detailRepo.findByPONo("PO20240115001")
      details should not be empty
      details.head.qty shouldBe 100
    }
  }

  it should "発注番号で検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val poRepo = PurchaseOrderRepository()

    DB localTx { implicit session =>
      setupTestData("02")

      val po = PurchaseOrder(
        poNo = "PO20240115002",
        poDate = LocalDateTime.of(2024, 1, 15, 10, 0),
        deptCode = "110002",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
        supCode = "SUP02",
        supSubNo = 0,
        empCode = "E02",
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin",
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
      setupTestData("03")

      val po1 = PurchaseOrder(
        poNo = "PO20240115003",
        poDate = LocalDateTime.of(2024, 1, 15, 10, 0),
        deptCode = "110003",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
        supCode = "SUP03",
        supSubNo = 0,
        empCode = "E03",
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin",
      )

      val po2 = PurchaseOrder(
        poNo = "PO20240115004",
        poDate = LocalDateTime.of(2024, 1, 16, 10, 0),
        deptCode = "110003",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
        supCode = "SUP03",
        supSubNo = 0,
        empCode = "E03",
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin",
      )

      poRepo.create(po1)
      poRepo.create(po2)

      val pos = poRepo.findBySupplier("SUP03", 0)
      pos should have size 2
      pos.map(_.poNo) should contain allOf ("PO20240115003", "PO20240115004")
    }
  }

  it should "発注を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val poRepo = PurchaseOrderRepository()

    DB localTx { implicit session =>
      setupTestData("05")

      val po = PurchaseOrder(
        poNo = "PO20240115005",
        poDate = LocalDateTime.of(2024, 1, 15, 10, 0),
        deptCode = "110005",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
        supCode = "SUP05",
        supSubNo = 0,
        empCode = "E05",
        completeFlg = 0,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin",
      )

      poRepo.create(po)

      val updated = po.copy(
        completeFlg = 1,
        updateDate = LocalDateTime.now(),
        updater = "admin2",
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
      setupTestData("06")

      val po = PurchaseOrder(
        poNo = "PO20240115006",
        poDate = LocalDateTime.of(2024, 1, 15, 10, 0),
        deptCode = "110006",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
        supCode = "SUP06",
        supSubNo = 0,
        empCode = "E06",
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin",
      )

      poRepo.create(po)

      val deleteResult = poRepo.delete("PO20240115006")
      deleteResult shouldBe 1

      val retrieved = poRepo.findByNo("PO20240115006")
      retrieved shouldBe None
    }
  }
}
