package com.example.accounting

import com.example.db.DatabaseSpec
import com.example.accounting.domain.account.TaxTransaction
import com.example.accounting.infrastructure.out.persistence.account.TaxTransactionRepository
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*
import java.time.LocalDateTime

/**
 * 課税取引マスタのテスト
 */
class TaxTransactionSpec extends DatabaseSpec with BeforeAndAfterEach:

  val repository = new TaxTransactionRepository()

  override def beforeEach(): Unit =
    super.beforeEach()

  behavior of "課税取引マスタ"

  it should "課税取引を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val tax = TaxTransaction(
        taxCode = "06",
        taxName = "テスト課税区分",
        taxRate = BigDecimal("0.05"),
        createdAt = LocalDateTime.now(),
        updatedAt = LocalDateTime.now(),
      )

      val result = repository.insert(tax)
      result shouldBe 1

      val found = repository.findByCode("06")
      found should not be None
      found.get.taxName shouldBe "テスト課税区分"
      found.get.taxRate shouldBe BigDecimal("0.050")
    }
  }

  it should "すべての課税取引を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.readOnly { implicit session =>
      val all = repository.findAll()
      all.size should be >= 5 // 初期データ5件以上
      all.map(_.taxCode) should contain allOf ("01", "02", "03", "04", "05")
    }
  }

  it should "コードで検索できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.readOnly { implicit session =>
      val tax = repository.findByCode("01")
      tax should not be None
      tax.get.taxName shouldBe "課税売上（標準税率）"
      tax.get.taxRate shouldBe BigDecimal("0.100")
    }
  }

  it should "課税取引を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val tax = repository.findByCode("01").get
      val updated = tax.copy(taxRate = BigDecimal("0.110"))

      repository.update(updated)

      val found = repository.findByCode("01").get
      found.taxRate shouldBe BigDecimal("0.110")
    }
  }

  it should "課税取引を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // テストデータを追加
      val tax = TaxTransaction(
        taxCode = "99",
        taxName = "削除テスト",
        taxRate = BigDecimal("0.00"),
        createdAt = LocalDateTime.now(),
        updatedAt = LocalDateTime.now(),
      )
      repository.insert(tax)

      val deleted = repository.delete("99")
      deleted shouldBe 1

      val found = repository.findByCode("99")
      found shouldBe None
    }
  }
