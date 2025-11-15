package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.entity.{Company, CompanyGroup, Supplier}
import scalikejdbc._

import java.time.LocalDateTime

class SupplierRepositorySpec extends DatabaseSpec {

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

  "SupplierRepository" should "仕入先を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = SupplierRepository()

    DB localTx { implicit session =>
      setupTestData("G301", "SUP0001")

      val supplier = Supplier(
        supCode = "SUP0001",
        supSubNo = 1,
        supType = 1,
        name = Some("株式会社テスト仕入先"),
        kana = Some("カブシキガイシャテストシイレサキ"),
        empCode = Some("EMP001"),
        contactPerson = Some("鈴木一郎"),
        contactDept = Some("購買部"),
        zipCode = Some("100-0001"),
        state = Some("東京都"),
        address1 = Some("港区芝公園1-1"),
        tel = Some("03-9876-5432"),
        email = Some("supplier@example.com"),
        closeDate = Some(31),
        payMonths = Some(1),
        payDate = Some(31),
        payMethod = Some(2),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      val result = repo.create(supplier)
      result shouldBe 1

      val retrieved = repo.findById("SUP0001", 1)
      retrieved.isDefined shouldBe true
      retrieved.get.name shouldBe Some("株式会社テスト仕入先")
      retrieved.get.closeDate shouldBe Some(31)
    }
  }

  it should "複合キーで仕入先を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = SupplierRepository()

    DB localTx { implicit session =>
      setupTestData("G302", "SUP0002")

      val supplier = Supplier(
        supCode = "SUP0002",
        supSubNo = 2,
        name = Some("XYZ物産"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(supplier)

      val retrieved = repo.findById("SUP0002", 2)
      retrieved.isDefined shouldBe true
      retrieved.get.supSubNo shouldBe 2
      retrieved.get.name shouldBe Some("XYZ物産")

      val notFound = repo.findById("SUP0002", 999)
      notFound shouldBe None
    }
  }

  it should "会社コードで全仕入先枝番を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = SupplierRepository()

    DB localTx { implicit session =>
      setupTestData("G303", "SUP0003")

      val supplier1 = Supplier(
        supCode = "SUP0003",
        supSubNo = 1,
        name = Some("本社工場"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      val supplier2 = Supplier(
        supCode = "SUP0003",
        supSubNo = 2,
        name = Some("大阪工場"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      val supplier3 = Supplier(
        supCode = "SUP0003",
        supSubNo = 3,
        name = Some("名古屋工場"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(supplier1)
      repo.create(supplier2)
      repo.create(supplier3)

      val suppliers = repo.findByCompany("SUP0003")
      suppliers should have size 3
      suppliers.map(_.supSubNo) shouldBe List(1, 2, 3)
      suppliers.map(_.name) shouldBe List(Some("本社工場"), Some("大阪工場"), Some("名古屋工場"))
    }
  }

  it should "全仕入先を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = SupplierRepository()

    DB localTx { implicit session =>
      setupTestData("G304", "SUP0004")

      val supplier = Supplier(
        supCode = "SUP0004",
        supSubNo = 1,
        name = Some("サンプル株式会社"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(supplier)

      val suppliers = repo.findAll()
      suppliers should not be empty
      suppliers.exists(s => s.supCode == "SUP0004" && s.supSubNo == 1) shouldBe true
    }
  }

  it should "仕入先を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = SupplierRepository()

    DB localTx { implicit session =>
      setupTestData("G305", "SUP0005")

      val supplier = Supplier(
        supCode = "SUP0005",
        supSubNo = 1,
        name = Some("更新前株式会社"),
        closeDate = Some(31),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(supplier)

      val updated = supplier.copy(
        name = Some("更新後株式会社"),
        closeDate = Some(25),
        updateDate = LocalDateTime.now(),
        updater = "admin2",
      )

      val updateResult = repo.update(updated)
      updateResult shouldBe 1

      val retrieved = repo.findById("SUP0005", 1)
      retrieved.isDefined shouldBe true
      retrieved.get.name shouldBe Some("更新後株式会社")
      retrieved.get.closeDate shouldBe Some(25)
      retrieved.get.updater shouldBe "admin2"
    }
  }

  it should "仕入先を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = SupplierRepository()

    DB localTx { implicit session =>
      setupTestData("G306", "SUP0006")

      val supplier = Supplier(
        supCode = "SUP0006",
        supSubNo = 1,
        name = Some("削除対象株式会社"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      repo.create(supplier)

      val deleteResult = repo.delete("SUP0006", 1)
      deleteResult shouldBe 1

      val retrieved = repo.findById("SUP0006", 1)
      retrieved shouldBe None
    }
  }

  it should "取引先削除時に仕入先も削除される（CASCADE）" in withContainers { container =>
    setupWithMigrations(container)

    val supplierRepo = SupplierRepository()
    val companyRepo  = CompanyRepository()

    DB localTx { implicit session =>
      setupTestData("G307", "SUP0007")

      val supplier = Supplier(
        supCode = "SUP0007",
        supSubNo = 1,
        name = Some("CASCADE検証株式会社"),
        createDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2024, 1, 1, 0, 0, 0),
        updater = "admin",
      )

      supplierRepo.create(supplier)

      val retrievedBefore = supplierRepo.findById("SUP0007", 1)
      retrievedBefore.isDefined shouldBe true

      // 取引先を削除
      companyRepo.delete("SUP0007")

      // 仕入先も自動的に削除される
      val retrievedAfter = supplierRepo.findById("SUP0007", 1)
      retrievedAfter shouldBe None
    }
  }
}
