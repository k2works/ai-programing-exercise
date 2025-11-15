package com.example.repository

import com.example.db.DatabaseSpec
import com.example.domain.{Product, Stock, Warehouse}
import scalikejdbc._

import java.time.LocalDateTime

class StockRepositorySpec extends DatabaseSpec {

  private def setupTestData(whCode: String, prodCode: String)(implicit session: DBSession): Unit = {
    // 倉庫作成
    val warehouseRepo = WarehouseRepository()
    val warehouse = Warehouse(
      whCode = whCode,
      name = s"テスト倉庫${whCode}",
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    warehouseRepo.create(warehouse)

    // 商品作成
    val productRepo = ProductRepository()
    val product = Product(
      prodCode = prodCode,
      fullName = s"テスト商品正式名${prodCode}",
      name = s"テスト商品${prodCode}",
      kana = Some(s"テストショウヒン${prodCode}"),
      createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      creator = "admin",
      updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
      updater = "admin"
    )
    productRepo.create(product)
  }

  "StockRepository" should "在庫を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    val stockRepo = StockRepository()

    DB localTx { implicit session =>
      setupTestData("WH1", "ST001")

      val stock = Stock(
        whCode = "WH1",
        prodCode = "ST001",
        rotNo = "LOT20250115001",
        stockType = "1",
        qualityType = "G",
        actual = 100,
        valid = 100,
        lastDeliveryDate = Some(LocalDateTime.of(2025, 1, 10, 0, 0, 0)),
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      val result = stockRepo.create(stock)
      result shouldBe 1

      val retrieved = stockRepo.findById("WH1", "ST001", "LOT20250115001", "1", "G")
      retrieved.isDefined shouldBe true
      retrieved.get.actual shouldBe 100
      retrieved.get.valid shouldBe 100
    }
  }

  it should "複合主キーで在庫を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    val stockRepo = StockRepository()

    DB localTx { implicit session =>
      setupTestData("WH2", "ST002")

      val stock = Stock(
        whCode = "WH2",
        prodCode = "ST002",
        rotNo = "LOT20250115002",
        stockType = "1",
        qualityType = "G",
        actual = 50,
        valid = 30,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      stockRepo.create(stock)

      val retrieved = stockRepo.findById("WH2", "ST002", "LOT20250115002", "1", "G")
      retrieved.isDefined shouldBe true
      retrieved.get.actual shouldBe 50
      retrieved.get.valid shouldBe 30

      val notFound = stockRepo.findById("WH2", "ST002", "LOT99999999999", "1", "G")
      notFound shouldBe None
    }
  }

  it should "倉庫別に在庫を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val stockRepo = StockRepository()

    DB localTx { implicit session =>
      setupTestData("WH3", "ST003")

      val stock1 = Stock(
        whCode = "WH3",
        prodCode = "ST003",
        rotNo = "LOT20250115001",
        actual = 100,
        valid = 100,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      val stock2 = Stock(
        whCode = "WH3",
        prodCode = "ST003",
        rotNo = "LOT20250115002",
        actual = 200,
        valid = 200,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      stockRepo.create(stock1)
      stockRepo.create(stock2)

      val stocks = stockRepo.findByWarehouse("WH3")
      stocks should have size 2
      stocks.map(_.rotNo) should contain allOf ("LOT20250115001", "LOT20250115002")
    }
  }

  it should "商品別に在庫を検索できる" in withContainers { container =>
    setupWithMigrations(container)

    val stockRepo = StockRepository()

    DB localTx { implicit session =>
      setupTestData("WH4", "ST004")

      // WH5 の倉庫だけを追加作成（商品 ST004 は既に作成済み）
      val warehouseRepo = WarehouseRepository()
      val warehouse5 = Warehouse(
        whCode = "WH5",
        name = "テスト倉庫WH5",
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )
      warehouseRepo.create(warehouse5)

      val stock1 = Stock(
        whCode = "WH4",
        prodCode = "ST004",
        rotNo = "LOT20250115001",
        actual = 100,
        valid = 100,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      val stock2 = Stock(
        whCode = "WH5",
        prodCode = "ST004",
        rotNo = "LOT20250115001",
        actual = 200,
        valid = 200,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      stockRepo.create(stock1)
      stockRepo.create(stock2)

      val stocks = stockRepo.findByProduct("ST004")
      stocks should have size 2
      stocks.map(_.whCode) should contain allOf ("WH4", "WH5")
    }
  }

  it should "在庫数を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    val stockRepo = StockRepository()

    DB localTx { implicit session =>
      setupTestData("WH6", "ST005")

      val stock = Stock(
        whCode = "WH6",
        prodCode = "ST005",
        rotNo = "LOT20250115001",
        actual = 100,
        valid = 100,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      stockRepo.create(stock)

      val updated = stock.copy(
        actual = 150,
        valid = 120,
        lastDeliveryDate = Some(LocalDateTime.of(2025, 1, 16, 0, 0, 0)),
        updateDate = LocalDateTime.now(),
        updater = "admin2"
      )

      val updateResult = stockRepo.update(updated)
      updateResult shouldBe 1

      val retrieved = stockRepo.findById("WH6", "ST005", "LOT20250115001", "1", "G")
      retrieved.isDefined shouldBe true
      retrieved.get.actual shouldBe 150
      retrieved.get.valid shouldBe 120
      retrieved.get.updater shouldBe "admin2"
    }
  }

  it should "在庫を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    val stockRepo = StockRepository()

    DB localTx { implicit session =>
      setupTestData("WH7", "ST006")

      val stock = Stock(
        whCode = "WH7",
        prodCode = "ST006",
        rotNo = "LOT20250115001",
        actual = 100,
        valid = 100,
        createDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        creator = "admin",
        updateDate = LocalDateTime.of(2025, 1, 15, 10, 0, 0),
        updater = "admin"
      )

      stockRepo.create(stock)

      val deleteResult = stockRepo.delete("WH7", "ST006", "LOT20250115001", "1", "G")
      deleteResult shouldBe 1

      val retrieved = stockRepo.findById("WH7", "ST006", "LOT20250115001", "1", "G")
      retrieved shouldBe None
    }
  }
}
