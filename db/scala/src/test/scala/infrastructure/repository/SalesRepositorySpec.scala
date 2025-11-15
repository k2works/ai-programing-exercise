package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.domain._
import scalikejdbc._

import java.time.LocalDateTime

class SalesRepositorySpec extends DatabaseSpec {

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

    // 社員の作成
    val empRepo = EmployeeRepository()
    val emp = Employee(
      empCode = s"E$suffix",
      name = "山田太郎",
      kana = "ヤマダタロウ",
      loginPassword = "password",
      tel = "03-1111-1111",
      fax = "03-1111-1112",
      deptCode = s"1200$suffix",
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
      compCode = s"CUS$suffix",
      name = s"株式会社顧客$suffix",
      kana = Some("カブシキガイシャコキャク"),
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

    // 顧客の作成
    val custRepo = CustomerRepository()
    val customer = Customer(
      custCode = s"CUS$suffix",
      custSubNo = 0,
      custType = 0,
      arCode = s"CUS$suffix",
      arSubNo = 0,
      payerCode = s"CUS$suffix",
      payerSubNo = 0,
      name = Some(s"株式会社顧客$suffix"),
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    custRepo.create(customer)

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

    // 受注の作成
    val orderRepo = OrderRepository()
    val order = Order(
      orderNo = s"SO202401$suffix",
      orderDate = LocalDateTime.of(2024, 1, 15, 10, 0, 0),
      deptCode = s"1200$suffix",
      startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
      custCode = s"CUS$suffix",
      custSubNo = 0,
      empCode = s"E$suffix",
      requiredDate = Some(LocalDateTime.of(2024, 1, 20, 0, 0, 0)),
      orderAmnt = 100000,
      cmpTax = 10000,
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    orderRepo.create(order)
  }

  "SalesRepository" should "売上を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val salesRepo = SalesRepository()

    DB localTx { implicit session =>
      setupTestData("01")

      val sales = Sales(
        salesNo = "SL2024011501",
        salesDate = Some(LocalDateTime.of(2024, 1, 15, 14, 0)),
        salesType = 1,
        orderNo = Some("SO20240101"),
        deptCode = "120001",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
        compCode = "CUS01",
        salesAmnt = 100000,
        cmpTax = 10000,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = Some("admin"),
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = Some("admin")
      )

      val result = salesRepo.create(sales)
      result shouldBe 1

      val retrieved = salesRepo.findByNo("SL2024011501")
      retrieved.isDefined shouldBe true
      retrieved.get.salesAmnt shouldBe 100000
      retrieved.get.cmpTax shouldBe 10000
    }
  }

  it should "受注番号で売上を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val salesRepo = SalesRepository()

    DB localTx { implicit session =>
      setupTestData("02")

      val sales = Sales(
        salesNo = "SL2024011502",
        salesDate = Some(LocalDateTime.of(2024, 1, 15, 14, 0)),
        salesType = 1,
        orderNo = Some("SO20240102"),
        deptCode = "120002",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
        compCode = "CUS02",
        salesAmnt = 100000,
        cmpTax = 10000,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = Some("admin"),
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = Some("admin")
      )

      salesRepo.create(sales)

      val salesList = salesRepo.findByOrderNo("SO20240102")
      salesList should not be empty
      salesList.head.orderNo shouldBe Some("SO20240102")
    }
  }

  it should "売上を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val salesRepo = SalesRepository()

    DB localTx { implicit session =>
      setupTestData("03")

      val sales = Sales(
        salesNo = "SL2024011503",
        salesDate = Some(LocalDateTime.of(2024, 1, 15, 14, 0)),
        salesType = 1,
        orderNo = Some("SO20240103"),
        deptCode = "120003",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
        compCode = "CUS03",
        salesAmnt = 100000,
        cmpTax = 10000,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = Some("admin"),
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = Some("admin")
      )

      salesRepo.create(sales)

      val updated = sales.copy(
        salesAmnt = 150000,
        cmpTax = 15000,
        updateDate = LocalDateTime.now(),
        updater = Some("admin2")
      )

      val updateResult = salesRepo.update(updated)
      updateResult shouldBe 1

      val retrieved = salesRepo.findByNo("SL2024011503")
      retrieved.isDefined shouldBe true
      retrieved.get.salesAmnt shouldBe 150000
      retrieved.get.cmpTax shouldBe 15000
    }
  }

  it should "売上を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val salesRepo = SalesRepository()

    DB localTx { implicit session =>
      setupTestData("04")

      val sales = Sales(
        salesNo = "SL2024011504",
        salesDate = Some(LocalDateTime.of(2024, 1, 15, 14, 0)),
        salesType = 1,
        orderNo = Some("SO20240104"),
        deptCode = "120004",
        startDate = LocalDateTime.of(2024, 1, 1, 0, 0),
        compCode = "CUS04",
        salesAmnt = 100000,
        cmpTax = 10000,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = Some("admin"),
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = Some("admin")
      )

      salesRepo.create(sales)

      val deleteResult = salesRepo.delete("SL2024011504")
      deleteResult shouldBe 1

      val retrieved = salesRepo.findByNo("SL2024011504")
      retrieved shouldBe None
    }
  }
}
