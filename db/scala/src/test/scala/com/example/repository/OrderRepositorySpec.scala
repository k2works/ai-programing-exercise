package com.example.repository

import com.example.db.DatabaseSpec
import com.example.domain.{Company, CompanyGroup, Customer, Department, Employee, Order}
import scalikejdbc._

import java.time.LocalDateTime

class OrderRepositorySpec extends DatabaseSpec {

  private def setupTestData(
    groupCode: String,
    compCode: String,
    deptCode: String,
    empCode: String
  )(implicit session: DBSession): Unit = {
    // 取引先グループ作成
    val groupRepo = CompanyGroupRepository()
    val group = CompanyGroup(
      compGroupCode = groupCode,
      groupName = s"テストグループ${groupCode}",
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )
    groupRepo.create(group)

    // 取引先作成
    val companyRepo = CompanyRepository()
    val company = Company(
      compCode = compCode,
      name = s"テスト商事${compCode}",
      compGroupCode = groupCode,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )
    companyRepo.create(company)

    // 顧客作成
    val customerRepo = CustomerRepository()
    val customer = Customer(
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
      updater = "admin"
    )
    customerRepo.create(customer)

    // 部門作成
    val deptRepo = DepartmentRepository()
    val dept = Department(
      deptCode = deptCode,
      startDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      endDate = LocalDateTime.of(2099, 12, 31, 23, 59, 59),
      name = "営業部",
      layer = 1,
      path = s"/${deptCode}/",
      lowestType = 1,
      slitYn = 1,
      createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
      updater = "admin"
    )
    deptRepo.create(dept)

    // 社員作成
    val empRepo = EmployeeRepository()
    val emp = Employee(
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
      updater = "admin"
    )
    empRepo.create(emp)
  }

  "OrderRepository" should "受注を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = OrderRepository()

    DB localTx { implicit session =>
      setupTestData("G401", "SO0001", "111000", "E0001")

      val order = Order(
        orderNo = "SO20250108001",
        orderDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        deptCode = "111000",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        custCode = "SO0001",
        custSubNo = 1,
        empCode = "E0001",
        requiredDate = Some(LocalDateTime.of(2025, 1, 15, 0, 0, 0)),
        custOrderNo = Some("CUST-ORDER-001"),
        whCode = None,
        orderAmnt = 50000,
        cmpTax = 5000,
        slipComment = Some("初回注文"),
        createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        updater = "admin"
      )

      val result = repo.create(order)
      result shouldBe 1

      val retrieved = repo.findById("SO20250108001")
      retrieved.isDefined shouldBe true
      retrieved.get.custCode shouldBe "SO0001"
      retrieved.get.orderAmnt shouldBe 50000
    }
  }

  it should "受注番号で受注を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = OrderRepository()

    DB localTx { implicit session =>
      setupTestData("G402", "SO0002", "112000", "E0002")

      val order = Order(
        orderNo = "SO20250108002",
        orderDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        deptCode = "112000",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        custCode = "SO0002",
        custSubNo = 1,
        empCode = "E0002",
        orderAmnt = 30000,
        cmpTax = 3000,
        createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        updater = "admin"
      )

      repo.create(order)

      val retrieved = repo.findById("SO20250108002")
      retrieved.isDefined shouldBe true
      retrieved.get.orderNo shouldBe "SO20250108002"
      retrieved.get.orderAmnt shouldBe 30000

      val notFound = repo.findById("SO99999999999")
      notFound shouldBe None
    }
  }

  it should "顧客別に受注を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = OrderRepository()

    DB localTx { implicit session =>
      setupTestData("G403", "SO0003", "113000", "E0003")

      val order1 = Order(
        orderNo = "SO20250108003",
        orderDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        deptCode = "113000",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        custCode = "SO0003",
        custSubNo = 1,
        empCode = "E0003",
        orderAmnt = 10000,
        cmpTax = 1000,
        createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        updater = "admin"
      )

      val order2 = Order(
        orderNo = "SO20250108004",
        orderDate = LocalDateTime.of(2025, 1, 9, 10, 0, 0),
        deptCode = "113000",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        custCode = "SO0003",
        custSubNo = 1,
        empCode = "E0003",
        orderAmnt = 20000,
        cmpTax = 2000,
        createDate = LocalDateTime.of(2025, 1, 9, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 9, 10, 0, 0),
        updater = "admin"
      )

      repo.create(order1)
      repo.create(order2)

      val orders = repo.findByCustomer("SO0003", 1)
      orders should have size 2
      orders.map(_.orderNo) should contain allOf ("SO20250108003", "SO20250108004")
      // 受注日降順でソートされているか確認
      orders.head.orderDate shouldBe LocalDateTime.of(2025, 1, 9, 10, 0, 0)
    }
  }

  it should "全受注を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = OrderRepository()

    DB localTx { implicit session =>
      setupTestData("G404", "SO0004", "114000", "E0004")

      val order = Order(
        orderNo = "SO20250108005",
        orderDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        deptCode = "114000",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        custCode = "SO0004",
        custSubNo = 1,
        empCode = "E0004",
        orderAmnt = 15000,
        cmpTax = 1500,
        createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        updater = "admin"
      )

      repo.create(order)

      val orders = repo.findAll()
      orders should not be empty
      orders.exists(_.orderNo == "SO20250108005") shouldBe true
    }
  }

  it should "受注を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = OrderRepository()

    DB localTx { implicit session =>
      setupTestData("G405", "SO0005", "115000", "E0005")

      val order = Order(
        orderNo = "SO20250108006",
        orderDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        deptCode = "115000",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        custCode = "SO0005",
        custSubNo = 1,
        empCode = "E0005",
        orderAmnt = 25000,
        cmpTax = 2500,
        slipComment = Some("更新前"),
        createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        updater = "admin"
      )

      repo.create(order)

      val updated = order.copy(
        orderAmnt = 30000,
        cmpTax = 3000,
        slipComment = Some("更新後"),
        updateDate = LocalDateTime.now(),
        updater = "admin2"
      )

      val updateResult = repo.update(updated)
      updateResult shouldBe 1

      val retrieved = repo.findById("SO20250108006")
      retrieved.isDefined shouldBe true
      retrieved.get.orderAmnt shouldBe 30000
      retrieved.get.cmpTax shouldBe 3000
      retrieved.get.slipComment shouldBe Some("更新後")
      retrieved.get.updater shouldBe "admin2"
    }
  }

  it should "受注を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = OrderRepository()

    DB localTx { implicit session =>
      setupTestData("G406", "SO0006", "116000", "E0006")

      val order = Order(
        orderNo = "SO20250108007",
        orderDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        deptCode = "116000",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        custCode = "SO0006",
        custSubNo = 1,
        empCode = "E0006",
        orderAmnt = 35000,
        cmpTax = 3500,
        createDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 8, 10, 0, 0),
        updater = "admin"
      )

      repo.create(order)

      val deleteResult = repo.delete("SO20250108007")
      deleteResult shouldBe 1

      val retrieved = repo.findById("SO20250108007")
      retrieved shouldBe None
    }
  }
}
