package com.example.accounting

import com.example.db.DatabaseSpec
import com.example.accounting.domain.account.{Account, AccountType}
import com.example.accounting.infrastructure.persistence.account.AccountRepository
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*
import java.time.LocalDateTime

/**
 * 勘定科目リポジトリのテスト
 */
class AccountRepositorySpec extends DatabaseSpec with BeforeAndAfterEach:

  val repository = new AccountRepository()

  override def beforeEach(): Unit =
    super.beforeEach()

  behavior of "勘定科目リポジトリ"

  it should "勘定科目を登録できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      val account = Account(
        accountId = None,
        accountCode = "R1000",
        accountName = "現金",
        accountType = AccountType.資産,
        balance = BigDecimal("50000.00"),
        bsplDistinction = Some("B"),
        transactionElement = Some("1"),
        expenseDistinction = None,
        isSummaryAccount = false,
        displayOrder = Some(100),
        isAggregationTarget = true,
        accountKana = Some("ゲンキン"),
        taxCode = Some("01"),
        createdAt = LocalDateTime.now(),
        updatedAt = LocalDateTime.now(),
      )

      val result = repository.insert(account)
      result shouldBe 1

      val found = repository.findByCode("R1000")
      found should not be None
      found.get.accountName shouldBe "現金"
      found.get.accountType shouldBe AccountType.資産
      found.get.balance shouldBe BigDecimal("50000.00")
    }
  }

  it should "すべての勘定科目を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      insertTestAccountsUnique("B")

      val all = repository.findAll()
      all.size should be >= 3
      all.map(_.accountCode) should contain allOf ("B1000", "B2000", "B3000")
    }
  }

  it should "勘定科目種別で検索できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      insertTestAccountsUnique("C")

      val assets = repository.findByType(AccountType.資産)
      assets should not be empty
      assets.foreach(_.accountType shouldBe AccountType.資産)
    }
  }

  it should "BSPL区分で検索できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      insertTestAccountsUnique("D")

      val bsAccounts = repository.findByBsplDistinction("B")
      bsAccounts should not be empty
      bsAccounts.foreach(_.bsplDistinction shouldBe Some("B"))
    }
  }

  it should "集計対象の勘定科目を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      insertTestAccountsUnique("E")

      val targets = repository.findAggregationTargets()
      targets should not be empty
      targets.foreach(_.isAggregationTarget shouldBe true)
    }
  }

  it should "合計科目を取得できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      insertTestAccountsUnique("F")
      insertSummaryAccountUnique("F")

      val summaries = repository.findSummaryAccounts()
      summaries should not be empty
      summaries.foreach(_.isSummaryAccount shouldBe true)
    }
  }

  it should "課税取引コードで検索できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      insertTestAccountsUnique("G")

      val taxed = repository.findByTaxCode("01")
      taxed should not be empty
      taxed.foreach(_.taxCode shouldBe Some("01"))
    }
  }

  it should "勘定科目カナで前方一致検索できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      insertTestAccountsUnique("H")

      val found = repository.findByKanaPrefix("ゲン")
      found should not be empty
      found.exists(_.accountKana.contains("ゲンキン")) shouldBe true
    }
  }

  it should "勘定科目を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      insertTestAccountsUnique("I")

      val account = repository.findByCode("I1000").get
      val updated = account.copy(accountName = "現金及び預金", balance = BigDecimal("100000.00"))

      repository.update(updated)

      val found = repository.findByCode("I1000").get
      found.accountName shouldBe "現金及び預金"
      found.balance shouldBe BigDecimal("100000.00")
    }
  }

  it should "残高を更新できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      insertTestAccountsUnique("J")

      repository.updateBalance("J1000", BigDecimal("75000.00"))

      val found = repository.findByCode("J1000").get
      found.balance shouldBe BigDecimal("75000.00")
    }
  }

  it should "勘定科目を削除できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      // 削除用テストデータを追加
      val account = Account(
        accountId = None,
        accountCode = "K9999",
        accountName = "削除テスト",
        accountType = AccountType.資産,
        balance = BigDecimal("0.00"),
        bsplDistinction = None,
        transactionElement = None,
        expenseDistinction = None,
        isSummaryAccount = false,
        displayOrder = None,
        isAggregationTarget = false,
        accountKana = None,
        taxCode = None,
        createdAt = LocalDateTime.now(),
        updatedAt = LocalDateTime.now(),
      )
      repository.insert(account)

      val deleted = repository.delete("K9999")
      deleted shouldBe 1

      val found = repository.findByCode("K9999")
      found shouldBe None
    }
  }

  it should "借方科目・貸方科目を判定できる" in withContainers { container =>
    setupWithMigrations(container)

    DB.localTx { implicit session =>
      insertTestAccountsUnique("L")

      val asset = repository.findByCode("L1000").get
      asset.isDebitAccount shouldBe true
      asset.isCreditAccount shouldBe false

      val liability = repository.findByCode("L2000").get
      liability.isDebitAccount shouldBe false
      liability.isCreditAccount shouldBe true
    }
  }

  // ヘルパーメソッド
  private def insertTestAccountsUnique(prefix: String)(implicit session: DBSession): Unit =
    val accounts = List(
      (s"${prefix}1000", "現金", "資産", BigDecimal("50000.00"), "B", "1", None, false, 100, true, "ゲンキン", "01"),
      (s"${prefix}2000", "買掛金", "負債", BigDecimal("30000.00"), "B", "2", None, false, 200, true, "カイカケキン", "01"),
      (s"${prefix}3000", "資本金", "純資産", BigDecimal("100000.00"), "B", "3", None, false, 300, true, "シホンキン", "03"),
    )
    accounts.foreach { case (code, name, typ, balance, bspl, elem, exp, summ, ord, agg, kana, tax) =>
      sql"""
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高",
         "BSPL区分", "取引要素区分", "費用区分", "合計科目",
         "表示順序", "集計対象", "勘定科目カナ", "課税取引コード")
        VALUES (
          ${code}, ${name}, ${typ}::account_type, ${balance},
          ${bspl}, ${elem}, ${exp}, ${summ},
          ${ord}, ${agg}, ${kana}, ${tax}
        )
      """.update.apply()
    }

  private def insertSummaryAccountUnique(prefix: String)(implicit session: DBSession): Unit =
    sql"""
      INSERT INTO "勘定科目マスタ"
      ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高",
       "BSPL区分", "取引要素区分", "費用区分", "合計科目",
       "表示順序", "集計対象", "勘定科目カナ", "課税取引コード")
      VALUES (
        ${s"${prefix}S1"}, '流動資産合計', '資産'::account_type, ${BigDecimal("0.00")},
        'B', '1', null, true,
        50, false, 'リュウドウシサンゴウケイ', null
      )
    """.update.apply()
