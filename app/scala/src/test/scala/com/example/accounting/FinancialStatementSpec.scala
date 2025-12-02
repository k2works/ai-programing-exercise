package com.example.accounting

import com.example.accounting.application.service.FinancialStatementService
import com.example.accounting.domain.financial.*
import com.example.db.DatabaseSpec
import org.scalatest.BeforeAndAfterEach
import scalikejdbc.*

import java.time.LocalDate

/**
 * 財務諸表生成のテスト
 */
class FinancialStatementSpec extends DatabaseSpec with BeforeAndAfterEach:

  override def beforeEach(): Unit =
    super.beforeEach()

  private def clearTestData()(implicit session: DBSession): Unit =
    sql"""DELETE FROM "日次勘定科目残高"""".update.apply()
    sql"""DELETE FROM "月次勘定科目残高"""".update.apply()
    sql"""DELETE FROM "勘定科目マスタ"""".update.apply()

  private def setupBalanceSheetTestData()(implicit session: DBSession): Unit =
    // 貸借対照表科目（BSPL区分='B'）
    sql"""
      INSERT INTO "勘定科目マスタ" (
        "勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高"
      ) VALUES
      ('1110', '普通預金', '資産'::account_type, 'B', 0),
      ('1410', '建物', '資産'::account_type, 'B', 0),
      ('2110', '買掛金', '負債'::account_type, 'B', 0),
      ('2510', '長期借入金', '負債'::account_type, 'B', 0),
      ('3110', '資本金', '純資産'::account_type, 'B', 0)
    """.update.apply()

    // 日次残高データ（2024-01-31時点）
    val asOfDate = LocalDate.of(2024, 1, 31)
    sql"""
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES
      (${asOfDate}, '1110', '', '', '', 0, 8000000, 0),
      (${asOfDate}, '1410', '', '', '', 0, 2000000, 0),
      (${asOfDate}, '2110', '', '', '', 0, 0, 500000),
      (${asOfDate}, '2510', '', '', '', 0, 0, 4500000),
      (${asOfDate}, '3110', '', '', '', 0, 0, 5000000)
    """.update.apply()

  private def setupIncomeStatementTestData()(implicit session: DBSession): Unit =
    // 損益計算書科目（BSPL区分='P'）
    sql"""
      INSERT INTO "勘定科目マスタ" (
        "勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高"
      ) VALUES
      ('4110', '売上高', '収益'::account_type, 'P', 0),
      ('5110', '売上原価', '費用'::account_type, 'P', 0),
      ('6110', '給料手当', '費用'::account_type, 'P', 0),
      ('6210', '地代家賃', '費用'::account_type, 'P', 0)
    """.update.apply()

    // 日次残高データ（2024年1月）
    sql"""
      INSERT INTO "日次勘定科目残高" (
        "起票日", "勘定科目コード", "補助科目コード", "部門コード",
        "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
      ) VALUES
      ('2024-01-15', '4110', '', '', '', 0, 0, 10000000),
      ('2024-01-15', '5110', '', '', '', 0, 6000000, 0),
      ('2024-01-31', '6110', '', '', '', 0, 2000000, 0),
      ('2024-01-31', '6210', '', '', '', 0, 500000, 0)
    """.update.apply()

  behavior of "貸借対照表生成"

  it should "貸借対照表を生成できる" in withContainers { container =>
    setupWithMigrations(container)
    val service = new FinancialStatementService()

    DB.localTx { implicit session =>
      clearTestData()
      setupBalanceSheetTestData()
    }

    DB.readOnly { implicit session =>
      val asOfDate = LocalDate.of(2024, 1, 31)
      val balanceSheet = service.generateBalanceSheet(asOfDate)

      balanceSheet.asOfDate shouldBe asOfDate
      balanceSheet.assets should not be empty
      balanceSheet.liabilities should not be empty
      balanceSheet.equity should not be empty
      balanceSheet.totalAssets should be > BigDecimal(0)
      balanceSheet.totalLiabilities should be > BigDecimal(0)
      balanceSheet.totalEquity should be > BigDecimal(0)
    }
  }

  it should "貸借平均の原則が成立している" in withContainers { container =>
    setupWithMigrations(container)
    val service = new FinancialStatementService()

    DB.localTx { implicit session =>
      clearTestData()
      setupBalanceSheetTestData()
    }

    DB.readOnly { implicit session =>
      val asOfDate = LocalDate.of(2024, 1, 31)
      val balanceSheet = service.generateBalanceSheet(asOfDate)

      // 資産 = 負債 + 純資産
      balanceSheet.totalAssets shouldBe (balanceSheet.totalLiabilities + balanceSheet.totalEquity)
      balanceSheet.isBalanced shouldBe true

      // 具体的な金額の検証
      // 資産: 普通預金8,000,000 + 建物2,000,000 = 10,000,000
      balanceSheet.totalAssets shouldBe BigDecimal("10000000")

      // 負債: 買掛金500,000 + 長期借入金4,500,000 = 5,000,000
      balanceSheet.totalLiabilities shouldBe BigDecimal("5000000")

      // 純資産: 資本金5,000,000
      balanceSheet.totalEquity shouldBe BigDecimal("5000000")
    }
  }

  it should "資産項目が正しく分類されている" in withContainers { container =>
    setupWithMigrations(container)
    val service = new FinancialStatementService()

    DB.localTx { implicit session =>
      clearTestData()
      setupBalanceSheetTestData()
    }

    DB.readOnly { implicit session =>
      val asOfDate = LocalDate.of(2024, 1, 31)
      val balanceSheet = service.generateBalanceSheet(asOfDate)

      val assets = balanceSheet.assets
      assets should have size 2

      // 普通預金
      val ordinaryDeposit = assets.find(_.accountCode == "1110")
      ordinaryDeposit shouldBe defined
      ordinaryDeposit.get.accountName shouldBe "普通預金"
      ordinaryDeposit.get.balance shouldBe BigDecimal("8000000")

      // 建物
      val building = assets.find(_.accountCode == "1410")
      building shouldBe defined
      building.get.accountName shouldBe "建物"
      building.get.balance shouldBe BigDecimal("2000000")
    }
  }

  it should "負債項目が正しく分類されている" in withContainers { container =>
    setupWithMigrations(container)
    val service = new FinancialStatementService()

    DB.localTx { implicit session =>
      clearTestData()
      setupBalanceSheetTestData()
    }

    DB.readOnly { implicit session =>
      val asOfDate = LocalDate.of(2024, 1, 31)
      val balanceSheet = service.generateBalanceSheet(asOfDate)

      val liabilities = balanceSheet.liabilities
      liabilities should have size 2

      // 買掛金
      val accountsPayable = liabilities.find(_.accountCode == "2110")
      accountsPayable shouldBe defined
      accountsPayable.get.balance shouldBe BigDecimal("500000")

      // 長期借入金
      val longTermDebt = liabilities.find(_.accountCode == "2510")
      longTermDebt shouldBe defined
      longTermDebt.get.balance shouldBe BigDecimal("4500000")
    }
  }

  it should "純資産項目が正しく分類されている" in withContainers { container =>
    setupWithMigrations(container)
    val service = new FinancialStatementService()

    DB.localTx { implicit session =>
      clearTestData()
      setupBalanceSheetTestData()
    }

    DB.readOnly { implicit session =>
      val asOfDate = LocalDate.of(2024, 1, 31)
      val balanceSheet = service.generateBalanceSheet(asOfDate)

      val equity = balanceSheet.equity
      equity should have size 1
      equity.head.accountCode shouldBe "3110"
      equity.head.accountName shouldBe "資本金"
      equity.head.balance shouldBe BigDecimal("5000000")
    }
  }

  it should "構成比率が計算されている" in withContainers { container =>
    setupWithMigrations(container)
    val service = new FinancialStatementService()

    DB.localTx { implicit session =>
      clearTestData()
      setupBalanceSheetTestData()
    }

    DB.readOnly { implicit session =>
      val asOfDate = LocalDate.of(2024, 1, 31)
      val balanceSheet = service.generateBalanceSheet(asOfDate)

      // 各項目の構成比率が0〜100の範囲内
      balanceSheet.assets.foreach { asset =>
        asset.percentage should be >= BigDecimal(0)
        asset.percentage should be <= BigDecimal(100)
      }

      // 普通預金の構成比率 = 8,000,000 / 10,000,000 × 100 = 80%
      val ordinaryDeposit = balanceSheet.assets.find(_.accountCode == "1110")
      ordinaryDeposit.get.percentage shouldBe BigDecimal("80.00")
    }
  }

  behavior of "損益計算書生成"

  it should "損益計算書を生成できる" in withContainers { container =>
    setupWithMigrations(container)
    val service = new FinancialStatementService()

    DB.localTx { implicit session =>
      clearTestData()
      setupIncomeStatementTestData()
    }

    DB.readOnly { implicit session =>
      val fromDate = LocalDate.of(2024, 1, 1)
      val toDate = LocalDate.of(2024, 1, 31)
      val incomeStatement = service.generateIncomeStatement(fromDate, toDate)

      incomeStatement.fromDate shouldBe fromDate
      incomeStatement.toDate shouldBe toDate
      incomeStatement.revenues should not be empty
      incomeStatement.expenses should not be empty
    }
  }

  it should "利益項目が正しく計算されている" in withContainers { container =>
    setupWithMigrations(container)
    val service = new FinancialStatementService()

    DB.localTx { implicit session =>
      clearTestData()
      setupIncomeStatementTestData()
    }

    DB.readOnly { implicit session =>
      val fromDate = LocalDate.of(2024, 1, 1)
      val toDate = LocalDate.of(2024, 1, 31)
      val incomeStatement = service.generateIncomeStatement(fromDate, toDate)

      // 総収益: 売上高10,000,000
      incomeStatement.totalRevenues shouldBe BigDecimal("10000000")

      // 総費用: 売上原価6,000,000 + 給料手当2,000,000 + 地代家賃500,000 = 8,500,000
      incomeStatement.totalExpenses shouldBe BigDecimal("8500000")

      // 売上総利益 = 売上高10,000,000 - 売上原価6,000,000 = 4,000,000
      incomeStatement.grossProfit shouldBe BigDecimal("4000000")

      // 営業利益 = 売上総利益4,000,000 - 販管費(給料+地代)2,500,000 = 1,500,000
      incomeStatement.operatingIncome shouldBe BigDecimal("1500000")

      // 当期純利益 = 収益10,000,000 - 費用8,500,000 = 1,500,000
      incomeStatement.netIncome shouldBe BigDecimal("1500000")
    }
  }

  it should "収益項目が正しく分類されている" in withContainers { container =>
    setupWithMigrations(container)
    val service = new FinancialStatementService()

    DB.localTx { implicit session =>
      clearTestData()
      setupIncomeStatementTestData()
    }

    DB.readOnly { implicit session =>
      val fromDate = LocalDate.of(2024, 1, 1)
      val toDate = LocalDate.of(2024, 1, 31)
      val incomeStatement = service.generateIncomeStatement(fromDate, toDate)

      incomeStatement.revenues should have size 1
      incomeStatement.revenues.head.accountCode shouldBe "4110"
      incomeStatement.revenues.head.accountName shouldBe "売上高"
      incomeStatement.revenues.head.balance shouldBe BigDecimal("10000000")
    }
  }

  it should "費用項目が正しく分類されている" in withContainers { container =>
    setupWithMigrations(container)
    val service = new FinancialStatementService()

    DB.localTx { implicit session =>
      clearTestData()
      setupIncomeStatementTestData()
    }

    DB.readOnly { implicit session =>
      val fromDate = LocalDate.of(2024, 1, 1)
      val toDate = LocalDate.of(2024, 1, 31)
      val incomeStatement = service.generateIncomeStatement(fromDate, toDate)

      incomeStatement.expenses should have size 3

      // 売上原価
      val costOfSales = incomeStatement.expenses.find(_.accountCode == "5110")
      costOfSales shouldBe defined
      costOfSales.get.balance shouldBe BigDecimal("6000000")

      // 給料手当
      val salary = incomeStatement.expenses.find(_.accountCode == "6110")
      salary shouldBe defined
      salary.get.balance shouldBe BigDecimal("2000000")

      // 地代家賃
      val rent = incomeStatement.expenses.find(_.accountCode == "6210")
      rent shouldBe defined
      rent.get.balance shouldBe BigDecimal("500000")
    }
  }

  it should "対売上比が計算されている" in withContainers { container =>
    setupWithMigrations(container)
    val service = new FinancialStatementService()

    DB.localTx { implicit session =>
      clearTestData()
      setupIncomeStatementTestData()
    }

    DB.readOnly { implicit session =>
      val fromDate = LocalDate.of(2024, 1, 1)
      val toDate = LocalDate.of(2024, 1, 31)
      val incomeStatement = service.generateIncomeStatement(fromDate, toDate)

      // 売上高の対売上比 = 100%
      incomeStatement.revenues.head.percentage shouldBe BigDecimal("100.00")

      // 売上原価の対売上比 = 6,000,000 / 10,000,000 × 100 = 60%
      val costOfSales = incomeStatement.expenses.find(_.accountCode == "5110")
      costOfSales.get.percentage shouldBe BigDecimal("60.00")
    }
  }

  behavior of "財務指標計算"

  it should "財務指標を計算できる" in withContainers { container =>
    setupWithMigrations(container)
    val service = new FinancialStatementService()

    DB.localTx { implicit session =>
      clearTestData()
      setupBalanceSheetTestData()
      setupIncomeStatementTestData()
    }

    DB.readOnly { implicit session =>
      val asOfDate = LocalDate.of(2024, 1, 31)
      val balanceSheet = service.generateBalanceSheet(asOfDate)

      val fromDate = LocalDate.of(2024, 1, 1)
      val toDate = LocalDate.of(2024, 1, 31)
      val incomeStatement = service.generateIncomeStatement(fromDate, toDate)

      val ratios = service.calculateFinancialRatios(balanceSheet, incomeStatement)

      // 流動比率 = 流動資産(普通預金8,000,000) / 流動負債(買掛金500,000) × 100 = 1600%
      ratios.currentRatio shouldBe BigDecimal("1600.00")

      // 自己資本比率 = 純資産(5,000,000) / 総資産(10,000,000) × 100 = 50%
      ratios.equityRatio shouldBe BigDecimal("50.00")

      // 売上総利益率 = 売上総利益(4,000,000) / 売上高(10,000,000) × 100 = 40%
      ratios.grossProfitMargin shouldBe BigDecimal("40.00")

      // 営業利益率 = 営業利益(1,500,000) / 売上高(10,000,000) × 100 = 15%
      ratios.operatingProfitMargin shouldBe BigDecimal("15.00")

      // 当期純利益率 = 当期純利益(1,500,000) / 売上高(10,000,000) × 100 = 15%
      ratios.netProfitMargin shouldBe BigDecimal("15.00")

      // ROA = 当期純利益(1,500,000) / 総資産(10,000,000) × 100 = 15%
      ratios.roa shouldBe BigDecimal("15.00")

      // ROE = 当期純利益(1,500,000) / 純資産(5,000,000) × 100 = 30%
      ratios.roe shouldBe BigDecimal("30.00")
    }
  }

  behavior of "ドメインモデル"

  it should "BalanceSheet の isBalanced が正しく動作する" in {
    val balancedSheet = BalanceSheet(
      asOfDate = LocalDate.of(2024, 1, 31),
      assets = List.empty,
      liabilities = List.empty,
      equity = List.empty,
      totalAssets = BigDecimal("10000000"),
      totalLiabilities = BigDecimal("5000000"),
      totalEquity = BigDecimal("5000000"),
      totalLiabilitiesAndEquity = BigDecimal("10000000"),
    )
    balancedSheet.isBalanced shouldBe true

    val unbalancedSheet = balancedSheet.copy(totalAssets = BigDecimal("9999999"))
    unbalancedSheet.isBalanced shouldBe false
  }
