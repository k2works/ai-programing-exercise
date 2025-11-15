package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.entity._
import scalikejdbc._

import java.time.LocalDateTime

class OrderDetailRepositorySpec extends DatabaseSpec {

  private def setupTestData(
    groupCode: String,
    compCode: String,
    deptCode: String,
    empCode: String,
    prodCode: String,
    orderNo: String,
  )(implicit session: DBSession): Unit = {
    // 取引先グループ作成
    val groupRepo = CompanyGroupRepository()
    val group     = CompanyGroup(
      compGroupCode = groupCode,
      groupName = s"テストグループ$groupCode",
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin",
    )
    groupRepo.create(group)

    // 取引先作成
    val companyRepo = CompanyRepository()
    val company     = Company(
      compCode = compCode,
      name = s"テスト商事$compCode",
      compGroupCode = groupCode,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin",
    )
    companyRepo.create(company)

    // 顧客作成
    val customerRepo = CustomerRepository()
    val customer     = Customer(
      custCode = compCode,
      custSubNo = 1,
      arCode = compCode,
      arSubNo = 1,
      payerCode = compCode,
      payerSubNo = 1,
      name = Some("テスト顧客"),
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin",
    )
    customerRepo.create(customer)

    // 部門作成
    val deptRepo = DepartmentRepository()
    val dept     = Department(
      deptCode = deptCode,
      startDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      endDate = LocalDateTime.of(2099, 12, 31, 23, 59, 59),
      name = "営業部",
      layer = 1,
      path = s"/$deptCode/",
      lowestType = 1,
      slitYn = 1,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin",
    )
    deptRepo.create(dept)

    // 社員作成
    val empRepo = EmployeeRepository()
    val emp     = Employee(
      empCode = empCode,
      name = "山田太郎",
      kana = "ヤマダタロウ",
      loginPassword = "password",
      tel = "03-1234-5678",
      fax = "03-1234-5679",
      deptCode = deptCode,
      startDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      occuCode = "01",
      approvalCode = "A1",
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin",
    )
    empRepo.create(emp)

    // 商品作成
    val productRepo = ProductRepository()
    val product     = Product(
      prodCode = prodCode,
      fullName = "テスト商品正式名",
      name = "テスト商品",
      kana = Some("テストショウヒン"),
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin",
    )
    productRepo.create(product)

    // 受注作成
    val orderRepo = OrderRepository()
    val order     = Order(
      orderNo = orderNo,
      orderDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
      deptCode = deptCode,
      startDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      custCode = compCode,
      custSubNo = 1,
      empCode = empCode,
      orderAmnt = 0,
      cmpTax = 0,
      createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
      updater = "admin",
    )
    orderRepo.create(order)
  }

  "OrderDetailRepository" should "受注明細を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = OrderDetailRepository()

    DB localTx { implicit session =>
      setupTestData("G501", "OD0001", "121000", "E5001", "P0001", "OD20250108001")

      val detail = OrderDetail(
        orderNo = "OD20250108001",
        soRowNo = 1,
        prodCode = "P0001",
        prodName = "テスト商品A",
        unitPrice = 1000,
        quantity = 10,
        cmpTaxRate = 10,
        reserveQty = 10,
        deliveryOrderQty = 0,
        deliveredQty = 0,
        completeFlg = 0,
        discount = 0,
        deliveryDate = Some(LocalDateTime.of(2025, 1, 15, 0, 0, 0)),
        createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        updater = "admin",
      )

      val result = repo.create(detail)
      result shouldBe 1

      val retrieved = repo.findById("OD20250108001", 1)
      retrieved.isDefined shouldBe true
      retrieved.get.prodCode shouldBe "P0001"
      retrieved.get.quantity shouldBe 10
    }
  }

  it should "受注番号で明細一覧を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = OrderDetailRepository()

    DB localTx { implicit session =>
      setupTestData("G502", "OD0002", "122000", "E5002", "P0002", "OD20250108002")

      val detail1 = OrderDetail(
        orderNo = "OD20250108002",
        soRowNo = 1,
        prodCode = "P0002",
        prodName = "テスト商品A",
        unitPrice = 1000,
        quantity = 5,
        createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        updater = "admin",
      )

      val detail2 = OrderDetail(
        orderNo = "OD20250108002",
        soRowNo = 2,
        prodCode = "P0002",
        prodName = "テスト商品B",
        unitPrice = 2000,
        quantity = 3,
        createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        updater = "admin",
      )

      repo.create(detail1)
      repo.create(detail2)

      val details = repo.findByOrderNo("OD20250108002")
      details should have size 2
      details.map(_.soRowNo) shouldBe List(1, 2)
    }
  }

  it should "明細を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = OrderDetailRepository()

    DB localTx { implicit session =>
      setupTestData("G503", "OD0003", "123000", "E5003", "P0003", "OD20250108003")

      val detail = OrderDetail(
        orderNo = "OD20250108003",
        soRowNo = 1,
        prodCode = "P0003",
        prodName = "テスト商品C",
        unitPrice = 1500,
        quantity = 10,
        reserveQty = 10,
        deliveryOrderQty = 0,
        deliveredQty = 0,
        completeFlg = 0,
        createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        updater = "admin",
      )

      repo.create(detail)

      val updated = detail.copy(
        deliveryOrderQty = 10,
        deliveredQty = 10,
        completeFlg = 1,
        updateDate = LocalDateTime.now(),
        updater = "admin2",
      )

      val updateResult = repo.update(updated)
      updateResult shouldBe 1

      val retrieved = repo.findById("OD20250108003", 1)
      retrieved.isDefined shouldBe true
      retrieved.get.deliveryOrderQty shouldBe 10
      retrieved.get.deliveredQty shouldBe 10
      retrieved.get.completeFlg shouldBe 1
      retrieved.get.updater shouldBe "admin2"
    }
  }

  it should "明細を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = OrderDetailRepository()

    DB localTx { implicit session =>
      setupTestData("G504", "OD0004", "124000", "E5004", "P0004", "OD20250108004")

      val detail = OrderDetail(
        orderNo = "OD20250108004",
        soRowNo = 1,
        prodCode = "P0004",
        prodName = "テスト商品D",
        unitPrice = 2000,
        quantity = 5,
        createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        updater = "admin",
      )

      repo.create(detail)

      val deleteResult = repo.delete("OD20250108004", 1)
      deleteResult shouldBe 1

      val retrieved = repo.findById("OD20250108004", 1)
      retrieved shouldBe None
    }
  }

  it should "受注削除時に明細も削除される（CASCADE）" in withContainers { container =>
    setupWithMigrations(container)

    val detailRepo = OrderDetailRepository()
    val orderRepo  = OrderRepository()

    DB localTx { implicit session =>
      setupTestData("G505", "OD0005", "125000", "E5005", "P0005", "OD20250108005")

      val detail = OrderDetail(
        orderNo = "OD20250108005",
        soRowNo = 1,
        prodCode = "P0005",
        prodName = "テスト商品E",
        unitPrice = 3000,
        quantity = 2,
        createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        updater = "admin",
      )

      detailRepo.create(detail)

      val retrievedBefore = detailRepo.findById("OD20250108005", 1)
      retrievedBefore.isDefined shouldBe true

      // 受注を削除
      orderRepo.delete("OD20250108005")

      // 明細も自動的に削除される
      val retrievedAfter = detailRepo.findById("OD20250108005", 1)
      retrievedAfter shouldBe None
    }
  }
}
