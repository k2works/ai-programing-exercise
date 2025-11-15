package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.entity.{Company, CompanyGroup, Customer}
import scalikejdbc._

import java.time.LocalDateTime

class CustomerRepositorySpec extends DatabaseSpec {

  private def setupTestData(groupCode: String, compCode: String)(implicit
    session: DBSession
  ): Unit = {
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
  }

  "CustomerRepository" should "顧客を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CustomerRepository()

    DB localTx { implicit session =>
      setupTestData("G201", "CUST0001")

      val customer = Customer(
        custCode = "CUST0001",
        custSubNo = 1,
        custType = 1,
        arCode = "CUST0001",
        arSubNo = 1,
        payerCode = "CUST0001",
        payerSubNo = 1,
        name = Some("株式会社テスト顧客"),
        kana = Some("カブシキガイシャテストコキャク"),
        empCode = Some("EMP001"),
        contactPerson = Some("山田太郎"),
        contactDept = Some("営業部"),
        zipCode = Some("100-0001"),
        state = Some("東京都"),
        address1 = Some("千代田区千代田1-1"),
        tel = Some("03-1234-5678"),
        email = Some("test@example.com"),
        closeDate1 = Some(31),
        payMonths1 = Some(1),
        payDate1 = Some(31),
        payMethod1 = Some(2),
        closeDate2 = Some(15),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      val result = repo.create(customer)
      result shouldBe 1

      val retrieved = repo.findById("CUST0001", 1)
      retrieved.isDefined shouldBe true
      retrieved.get.name shouldBe Some("株式会社テスト顧客")
      retrieved.get.closeDate1 shouldBe Some(31)
    }
  }

  it should "複合キーで顧客を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CustomerRepository()

    DB localTx { implicit session =>
      setupTestData("G202", "CUST0002")

      val customer = Customer(
        custCode = "CUST0002",
        custSubNo = 2,
        arCode = "CUST0002",
        arSubNo = 1,
        payerCode = "CUST0002",
        payerSubNo = 1,
        name = Some("ABC商事"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(customer)

      val retrieved = repo.findById("CUST0002", 2)
      retrieved.isDefined shouldBe true
      retrieved.get.custSubNo shouldBe 2
      retrieved.get.name shouldBe Some("ABC商事")

      val notFound = repo.findById("CUST0002", 999)
      notFound shouldBe None
    }
  }

  it should "会社コードで全顧客枝番を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CustomerRepository()

    DB localTx { implicit session =>
      setupTestData("G203", "CUST0003")

      val customer1 = Customer(
        custCode = "CUST0003",
        custSubNo = 1,
        arCode = "CUST0003",
        arSubNo = 1,
        payerCode = "CUST0003",
        payerSubNo = 1,
        name = Some("本社"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      val customer2 = Customer(
        custCode = "CUST0003",
        custSubNo = 2,
        arCode = "CUST0003",
        arSubNo = 1,
        payerCode = "CUST0003",
        payerSubNo = 1,
        name = Some("大阪支店"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      val customer3 = Customer(
        custCode = "CUST0003",
        custSubNo = 3,
        arCode = "CUST0003",
        arSubNo = 1,
        payerCode = "CUST0003",
        payerSubNo = 1,
        name = Some("名古屋支店"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(customer1)
      repo.create(customer2)
      repo.create(customer3)

      val customers = repo.findByCompany("CUST0003")
      customers should have size 3
      customers.map(_.custSubNo) shouldBe List(1, 2, 3)
      customers.map(_.name) shouldBe List(Some("本社"), Some("大阪支店"), Some("名古屋支店"))
    }
  }

  it should "全顧客を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CustomerRepository()

    DB localTx { implicit session =>
      setupTestData("G204", "CUST0004")

      val customer = Customer(
        custCode = "CUST0004",
        custSubNo = 1,
        arCode = "CUST0004",
        arSubNo = 1,
        payerCode = "CUST0004",
        payerSubNo = 1,
        name = Some("サンプル株式会社"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(customer)

      val customers = repo.findAll()
      customers should not be empty
      customers.exists(c => c.custCode == "CUST0004" && c.custSubNo == 1) shouldBe true
    }
  }

  it should "顧客を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CustomerRepository()

    DB localTx { implicit session =>
      setupTestData("G205", "CUST0005")

      val customer = Customer(
        custCode = "CUST0005",
        custSubNo = 1,
        arCode = "CUST0005",
        arSubNo = 1,
        payerCode = "CUST0005",
        payerSubNo = 1,
        name = Some("更新前株式会社"),
        closeDate1 = Some(31),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(customer)

      val updated = customer.copy(
        name = Some("更新後株式会社"),
        closeDate1 = Some(25),
        updateDate = LocalDateTime.now(),
        updater = "admin2",
      )

      val updateResult = repo.update(updated)
      updateResult shouldBe 1

      val retrieved = repo.findById("CUST0005", 1)
      retrieved.isDefined shouldBe true
      retrieved.get.name shouldBe Some("更新後株式会社")
      retrieved.get.closeDate1 shouldBe Some(25)
      retrieved.get.updater shouldBe "admin2"
    }
  }

  it should "顧客を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = CustomerRepository()

    DB localTx { implicit session =>
      setupTestData("G206", "CUST0006")

      val customer = Customer(
        custCode = "CUST0006",
        custSubNo = 1,
        arCode = "CUST0006",
        arSubNo = 1,
        payerCode = "CUST0006",
        payerSubNo = 1,
        name = Some("削除対象株式会社"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(customer)

      val deleteResult = repo.delete("CUST0006", 1)
      deleteResult shouldBe 1

      val retrieved = repo.findById("CUST0006", 1)
      retrieved shouldBe None
    }
  }

  it should "取引先削除時に顧客も削除される（CASCADE）" in withContainers { container =>
    setupWithMigrations(container)

    val customerRepo = CustomerRepository()
    val companyRepo  = CompanyRepository()

    DB localTx { implicit session =>
      setupTestData("G207", "CUST0007")

      val customer = Customer(
        custCode = "CUST0007",
        custSubNo = 1,
        arCode = "CUST0007",
        arSubNo = 1,
        payerCode = "CUST0007",
        payerSubNo = 1,
        name = Some("CASCADE検証株式会社"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      customerRepo.create(customer)

      val retrievedBefore = customerRepo.findById("CUST0007", 1)
      retrievedBefore.isDefined shouldBe true

      // 取引先を削除
      companyRepo.delete("CUST0007")

      // 顧客も自動的に削除される
      val retrievedAfter = customerRepo.findById("CUST0007", 1)
      retrievedAfter shouldBe None
    }
  }
}
