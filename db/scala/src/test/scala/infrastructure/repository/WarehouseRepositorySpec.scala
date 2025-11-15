package infrastructure.repository

import infrastructure.db.DatabaseSpec
import infrastructure.domain.Warehouse
import scalikejdbc._

import java.time.LocalDateTime

class WarehouseRepositorySpec extends DatabaseSpec {

  "WarehouseRepository" should "倉庫を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = WarehouseRepository()

    DB localTx { implicit session =>
      val warehouse = Warehouse(
        whCode = "WH1",
        name = "東京倉庫",
        zipCode = Some("100-0001"),
        state = Some("東京都"),
        address1 = Some("千代田区千代田1-1"),
        address2 = None,
        tel = Some("03-1234-5678"),
        fax = None,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      val result = repo.create(warehouse)
      result shouldBe 1

      val retrieved = repo.findById("WH1")
      retrieved.isDefined shouldBe true
      retrieved.get.name shouldBe "東京倉庫"
      retrieved.get.zipCode shouldBe Some("100-0001")
    }
  }

  it should "倉庫コードで倉庫を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = WarehouseRepository()

    DB localTx { implicit session =>
      val warehouse = Warehouse(
        whCode = "WH2",
        name = "大阪倉庫",
        zipCode = Some("530-0001"),
        state = Some("大阪府"),
        address1 = Some("大阪市北区梅田1-1"),
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      repo.create(warehouse)

      val retrieved = repo.findById("WH2")
      retrieved.isDefined shouldBe true
      retrieved.get.whCode shouldBe "WH2"
      retrieved.get.name shouldBe "大阪倉庫"

      val notFound = repo.findById("WH999")
      notFound shouldBe None
    }
  }

  it should "全倉庫を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = WarehouseRepository()

    DB localTx { implicit session =>
      val warehouse1 = Warehouse(
        whCode = "WH3",
        name = "名古屋倉庫",
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      val warehouse2 = Warehouse(
        whCode = "WH4",
        name = "福岡倉庫",
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      repo.create(warehouse1)
      repo.create(warehouse2)

      val warehouses = repo.findAll()
      warehouses should not be empty
      warehouses.size should be >= 2
      warehouses.map(_.whCode) should contain allOf ("WH3", "WH4")
    }
  }

  it should "倉庫を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = WarehouseRepository()

    DB localTx { implicit session =>
      val warehouse = Warehouse(
        whCode = "WH5",
        name = "札幌倉庫",
        tel = Some("011-111-1111"),
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      repo.create(warehouse)

      val updated = warehouse.copy(
        name = "札幌中央倉庫",
        tel = Some("011-222-2222"),
        updateDate = LocalDateTime.now(),
        updater = "admin2"
      )

      val updateResult = repo.update(updated)
      updateResult shouldBe 1

      val retrieved = repo.findById("WH5")
      retrieved.isDefined shouldBe true
      retrieved.get.name shouldBe "札幌中央倉庫"
      retrieved.get.tel shouldBe Some("011-222-2222")
      retrieved.get.updater shouldBe "admin2"
    }
  }

  it should "倉庫を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val repo = WarehouseRepository()

    DB localTx { implicit session =>
      val warehouse = Warehouse(
        whCode = "WH6",
        name = "仙台倉庫",
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      repo.create(warehouse)

      val deleteResult = repo.delete("WH6")
      deleteResult shouldBe 1

      val retrieved = repo.findById("WH6")
      retrieved shouldBe None
    }
  }
}
