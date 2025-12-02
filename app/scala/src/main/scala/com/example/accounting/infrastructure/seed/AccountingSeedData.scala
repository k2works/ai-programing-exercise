package com.example.accounting.infrastructure.seed

import java.time.LocalDate

/**
 * 財務会計システムの Seed データを提供するオブジェクト
 * D社（化粧品製造販売）の財務データを基に作成
 */
object AccountingSeedData:

  // 勘定科目開始日(全科目共通)
  val AccountStartDate: LocalDate = LocalDate.of(2021, 4, 1)

  /**
   * 勘定科目マスタデータ
   */
  case class AccountData(
    accountCode: String,
    accountName: String,
    accountType: String,
    level: Int,
    startDate: LocalDate
  )

  val accounts: List[AccountData] = List(
    // 資産の部
    AccountData("1", "資産", "資産", 1, AccountStartDate),
    AccountData("11", "流動資産", "資産", 2, AccountStartDate),
    AccountData("111", "現金預金", "資産", 3, AccountStartDate),
    AccountData("112", "売掛金", "資産", 3, AccountStartDate),
    AccountData("113", "売上債権", "資産", 3, AccountStartDate),
    AccountData("114", "棚卸資産", "資産", 3, AccountStartDate),
    AccountData("115", "その他流動資産", "資産", 3, AccountStartDate),
    AccountData("12", "固定資産", "資産", 2, AccountStartDate),
    AccountData("121", "有形固定資産", "資産", 3, AccountStartDate),
    AccountData("1211", "建物及び構築物", "資産", 4, AccountStartDate),
    AccountData("1212", "機械装置及び運搬具", "資産", 4, AccountStartDate),
    AccountData("1213", "工具器具備品", "資産", 4, AccountStartDate),
    AccountData("1214", "土地", "資産", 4, AccountStartDate),
    AccountData("1215", "その他有形固定資産", "資産", 4, AccountStartDate),
    AccountData("122", "無形固定資産", "資産", 3, AccountStartDate),
    AccountData("123", "投資その他の資産", "資産", 3, AccountStartDate),

    // 負債の部
    AccountData("2", "負債", "負債", 1, AccountStartDate),
    AccountData("21", "流動負債", "負債", 2, AccountStartDate),
    AccountData("211", "買掛金", "負債", 3, AccountStartDate),
    AccountData("212", "短期借入金", "負債", 3, AccountStartDate),
    AccountData("213", "未払金", "負債", 3, AccountStartDate),
    AccountData("214", "未払法人税等", "負債", 3, AccountStartDate),
    AccountData("215", "その他流動負債", "負債", 3, AccountStartDate),
    AccountData("22", "固定負債", "負債", 2, AccountStartDate),
    AccountData("221", "長期借入金", "負債", 3, AccountStartDate),
    AccountData("222", "リース債務", "負債", 3, AccountStartDate),

    // 純資産の部
    AccountData("3", "純資産", "純資産", 1, AccountStartDate),
    AccountData("31", "資本金", "純資産", 2, AccountStartDate),
    AccountData("32", "資本剰余金", "純資産", 2, AccountStartDate),
    AccountData("33", "利益剰余金", "純資産", 2, AccountStartDate),

    // 収益の部
    AccountData("4", "収益", "収益", 1, AccountStartDate),
    AccountData("41", "売上高", "収益", 2, AccountStartDate),
    AccountData("42", "営業外収益", "収益", 2, AccountStartDate),
    AccountData("43", "特別利益", "収益", 2, AccountStartDate),

    // 費用の部
    AccountData("5", "費用", "費用", 1, AccountStartDate),
    AccountData("51", "売上原価", "費用", 2, AccountStartDate),
    AccountData("52", "販売費及び一般管理費", "費用", 2, AccountStartDate),
    AccountData("53", "営業外費用", "費用", 2, AccountStartDate),
    AccountData("54", "特別損失", "費用", 2, AccountStartDate),
    AccountData("55", "法人税等", "費用", 2, AccountStartDate),
    AccountData("56", "当期純利益", "費用", 2, AccountStartDate)
  )

  /**
   * 勘定科目構成マスタデータ
   * チルダ(~)連結による階層パスを使用
   */
  case class AccountStructureData(
    accountCode: String,
    accountPath: String
  )

  val accountStructures: List[AccountStructureData] = List(
    // 資産の部
    AccountStructureData("1", "1"),
    AccountStructureData("11", "1~11"),
    AccountStructureData("111", "1~11~111"),
    AccountStructureData("112", "1~11~112"),
    AccountStructureData("113", "1~11~113"),
    AccountStructureData("114", "1~11~114"),
    AccountStructureData("115", "1~11~115"),
    AccountStructureData("12", "1~12"),
    AccountStructureData("121", "1~12~121"),
    AccountStructureData("1211", "1~12~121~1211"),
    AccountStructureData("1212", "1~12~121~1212"),
    AccountStructureData("1213", "1~12~121~1213"),
    AccountStructureData("1214", "1~12~121~1214"),
    AccountStructureData("1215", "1~12~121~1215"),
    AccountStructureData("122", "1~12~122"),
    AccountStructureData("123", "1~12~123"),

    // 負債の部
    AccountStructureData("2", "2"),
    AccountStructureData("21", "2~21"),
    AccountStructureData("211", "2~21~211"),
    AccountStructureData("212", "2~21~212"),
    AccountStructureData("213", "2~21~213"),
    AccountStructureData("214", "2~21~214"),
    AccountStructureData("215", "2~21~215"),
    AccountStructureData("22", "2~22"),
    AccountStructureData("221", "2~22~221"),
    AccountStructureData("222", "2~22~222"),

    // 純資産の部
    AccountStructureData("3", "3"),
    AccountStructureData("31", "3~31"),
    AccountStructureData("32", "3~32"),
    AccountStructureData("33", "3~33"),

    // 収益の部
    AccountStructureData("4", "4"),
    AccountStructureData("41", "4~41"),
    AccountStructureData("42", "4~42"),
    AccountStructureData("43", "4~43"),

    // 費用の部
    AccountStructureData("5", "5"),
    AccountStructureData("51", "5~51"),
    AccountStructureData("52", "5~52"),
    AccountStructureData("53", "5~53"),
    AccountStructureData("54", "5~54"),
    AccountStructureData("55", "5~55"),
    AccountStructureData("56", "5~56")
  )

  /**
   * 会計期間マスタデータ
   */
  case class AccountingPeriodData(
    fiscalYear: Int,
    periodName: String,
    startDate: LocalDate,
    endDate: LocalDate,
    isClosed: Boolean
  )

  val periods: List[AccountingPeriodData] = List(
    AccountingPeriodData(
      fiscalYear = 2021,
      periodName = "令和3年度",
      startDate = LocalDate.of(2021, 4, 1),
      endDate = LocalDate.of(2022, 3, 31),
      isClosed = true
    ),
    AccountingPeriodData(
      fiscalYear = 2022,
      periodName = "令和4年度",
      startDate = LocalDate.of(2022, 4, 1),
      endDate = LocalDate.of(2023, 3, 31),
      isClosed = true
    )
  )

  /**
   * 仕訳データ
   */
  case class JournalData(
    journalDate: LocalDate,
    fiscalYear: Int,
    description: String
  )

  val fy2021Journals: List[JournalData] = List(
    JournalData(
      journalDate = LocalDate.of(2022, 3, 31),
      fiscalYear = 2021,
      description = "令和3年度期末仕訳"
    )
  )

  val fy2022Journals: List[JournalData] = List(
    JournalData(
      journalDate = LocalDate.of(2023, 3, 31),
      fiscalYear = 2022,
      description = "令和4年度期末仕訳"
    )
  )

  /**
   * 仕訳明細データ
   */
  case class JournalEntryData(
    accountCode: String,
    debitAmount: BigDecimal,
    creditAmount: BigDecimal,
    description: String
  )

  // 令和3年度のD社財務データ（単位：千円）
  val fy2021Entries: List[JournalEntryData] = List(
    // 貸借対照表（資産）
    JournalEntryData("11", BigDecimal("2676193000"), BigDecimal(0), "流動資産"),
    JournalEntryData("12", BigDecimal("186973000"), BigDecimal(0), "固定資産"),
    // 貸借対照表（負債）
    JournalEntryData("21", BigDecimal(0), BigDecimal("851394000"), "流動負債"),
    JournalEntryData("22", BigDecimal(0), BigDecimal("22500000"), "固定負債"),
    // 貸借対照表（純資産）
    JournalEntryData("31", BigDecimal(0), BigDecimal("100000000"), "資本金"),
    JournalEntryData("33", BigDecimal(0), BigDecimal("1889272000"), "利益剰余金"),

    // 損益計算書（費用）
    JournalEntryData("51", BigDecimal("2185856000"), BigDecimal(0), "売上原価"),
    JournalEntryData("52", BigDecimal("2625222000"), BigDecimal(0), "販売費及び一般管理費"),
    JournalEntryData("53", BigDecimal("2676000"), BigDecimal(0), "営業外費用"),
    JournalEntryData("55", BigDecimal("331059000"), BigDecimal(0), "法人税等"),
    // 損益計算書（収益）
    JournalEntryData("41", BigDecimal(0), BigDecimal("5796105000"), "売上高"),
    JournalEntryData("42", BigDecimal(0), BigDecimal("368000"), "営業外収益"),
    JournalEntryData("56", BigDecimal(0), BigDecimal("651660000"), "当期純利益")
  )

  // 令和4年度のD社財務データ（単位：千円）
  val fy2022Entries: List[JournalEntryData] = List(
    // 貸借対照表（資産）
    JournalEntryData("11", BigDecimal("2777545000"), BigDecimal(0), "流動資産"),
    JournalEntryData("12", BigDecimal("197354000"), BigDecimal(0), "固定資産"),
    // 貸借対照表（負債）
    JournalEntryData("21", BigDecimal(0), BigDecimal("640513000"), "流動負債"),
    JournalEntryData("22", BigDecimal(0), BigDecimal("27153000"), "固定負債"),
    // 貸借対照表（純資産）
    JournalEntryData("31", BigDecimal(0), BigDecimal("100000000"), "資本金"),
    JournalEntryData("33", BigDecimal(0), BigDecimal("2207233000"), "利益剰余金"),

    // 損益計算書（費用）
    JournalEntryData("51", BigDecimal("1743821000"), BigDecimal(0), "売上原価"),
    JournalEntryData("52", BigDecimal("2277050000"), BigDecimal(0), "販売費及び一般管理費"),
    JournalEntryData("53", BigDecimal("1613000"), BigDecimal(0), "営業外費用"),
    JournalEntryData("55", BigDecimal("169072000"), BigDecimal(0), "法人税等"),
    // 損益計算書（収益）
    JournalEntryData("41", BigDecimal(0), BigDecimal("4547908000"), "売上高"),
    JournalEntryData("42", BigDecimal(0), BigDecimal("11608000"), "営業外収益"),
    JournalEntryData("56", BigDecimal(0), BigDecimal("367960000"), "当期純利益")
  )

  /**
   * 日次勘定科目残高データ
   */
  case class DailyBalanceData(
    balanceDate: LocalDate,
    accountCode: String,
    auxiliaryCode: String,
    departmentCode: String,
    projectCode: String,
    isClosingEntry: Boolean,
    debitAmount: BigDecimal,
    creditAmount: BigDecimal
  )

  val fy2021DailyBalances: List[DailyBalanceData] =
    val endDate = LocalDate.of(2022, 3, 31)
    List(
      DailyBalanceData(endDate, "11", "", "", "", true, BigDecimal("2676193000"), BigDecimal(0)),
      DailyBalanceData(endDate, "12", "", "", "", true, BigDecimal("186973000"), BigDecimal(0)),
      DailyBalanceData(endDate, "21", "", "", "", true, BigDecimal(0), BigDecimal("851394000")),
      DailyBalanceData(endDate, "22", "", "", "", true, BigDecimal(0), BigDecimal("22500000")),
      DailyBalanceData(endDate, "31", "", "", "", true, BigDecimal(0), BigDecimal("100000000")),
      DailyBalanceData(endDate, "33", "", "", "", true, BigDecimal(0), BigDecimal("1889272000")),
      DailyBalanceData(endDate, "51", "", "", "", true, BigDecimal("2185856000"), BigDecimal(0)),
      DailyBalanceData(endDate, "52", "", "", "", true, BigDecimal("2625222000"), BigDecimal(0)),
      DailyBalanceData(endDate, "53", "", "", "", true, BigDecimal("2676000"), BigDecimal(0)),
      DailyBalanceData(endDate, "55", "", "", "", true, BigDecimal("331059000"), BigDecimal(0)),
      DailyBalanceData(endDate, "41", "", "", "", true, BigDecimal(0), BigDecimal("5796105000")),
      DailyBalanceData(endDate, "42", "", "", "", true, BigDecimal(0), BigDecimal("368000")),
      DailyBalanceData(endDate, "56", "", "", "", true, BigDecimal(0), BigDecimal("651660000"))
    )

  val fy2022DailyBalances: List[DailyBalanceData] =
    val endDate = LocalDate.of(2023, 3, 31)
    List(
      DailyBalanceData(endDate, "11", "", "", "", true, BigDecimal("2777545000"), BigDecimal(0)),
      DailyBalanceData(endDate, "12", "", "", "", true, BigDecimal("197354000"), BigDecimal(0)),
      DailyBalanceData(endDate, "21", "", "", "", true, BigDecimal(0), BigDecimal("640513000")),
      DailyBalanceData(endDate, "22", "", "", "", true, BigDecimal(0), BigDecimal("27153000")),
      DailyBalanceData(endDate, "31", "", "", "", true, BigDecimal(0), BigDecimal("100000000")),
      DailyBalanceData(endDate, "33", "", "", "", true, BigDecimal(0), BigDecimal("2207233000")),
      DailyBalanceData(endDate, "51", "", "", "", true, BigDecimal("1743821000"), BigDecimal(0)),
      DailyBalanceData(endDate, "52", "", "", "", true, BigDecimal("2277050000"), BigDecimal(0)),
      DailyBalanceData(endDate, "53", "", "", "", true, BigDecimal("1613000"), BigDecimal(0)),
      DailyBalanceData(endDate, "55", "", "", "", true, BigDecimal("169072000"), BigDecimal(0)),
      DailyBalanceData(endDate, "41", "", "", "", true, BigDecimal(0), BigDecimal("4547908000")),
      DailyBalanceData(endDate, "42", "", "", "", true, BigDecimal(0), BigDecimal("11608000")),
      DailyBalanceData(endDate, "56", "", "", "", true, BigDecimal(0), BigDecimal("367960000"))
    )

  /**
   * 月次勘定科目残高データ
   */
  case class MonthlyBalanceData(
    fiscalYear: Int,
    month: Int,
    accountCode: String,
    auxiliaryCode: String,
    departmentCode: String,
    projectCode: String,
    beginningBalance: BigDecimal,
    debitAmount: BigDecimal,
    creditAmount: BigDecimal,
    endingBalance: BigDecimal
  )

  val fy2021MonthlyBalances: List[MonthlyBalanceData] = List(
    // 資産(借方残高)
    MonthlyBalanceData(2021, 3, "11", "", "", "", BigDecimal(0), BigDecimal("2676193000"), BigDecimal(0), BigDecimal("2676193000")),
    MonthlyBalanceData(2021, 3, "12", "", "", "", BigDecimal(0), BigDecimal("186973000"), BigDecimal(0), BigDecimal("186973000")),

    // 負債(貸方残高)
    MonthlyBalanceData(2021, 3, "21", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("851394000"), BigDecimal("851394000")),
    MonthlyBalanceData(2021, 3, "22", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("22500000"), BigDecimal("22500000")),

    // 純資産(貸方残高)
    MonthlyBalanceData(2021, 3, "31", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("100000000"), BigDecimal("100000000")),
    MonthlyBalanceData(2021, 3, "33", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("1889272000"), BigDecimal("1889272000")),

    // 費用(借方残高)
    MonthlyBalanceData(2021, 3, "51", "", "", "", BigDecimal(0), BigDecimal("2185856000"), BigDecimal(0), BigDecimal("2185856000")),
    MonthlyBalanceData(2021, 3, "52", "", "", "", BigDecimal(0), BigDecimal("2625222000"), BigDecimal(0), BigDecimal("2625222000")),
    MonthlyBalanceData(2021, 3, "53", "", "", "", BigDecimal(0), BigDecimal("2676000"), BigDecimal(0), BigDecimal("2676000")),
    MonthlyBalanceData(2021, 3, "55", "", "", "", BigDecimal(0), BigDecimal("331059000"), BigDecimal(0), BigDecimal("331059000")),

    // 収益(貸方残高)
    MonthlyBalanceData(2021, 3, "41", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("5796105000"), BigDecimal("5796105000")),
    MonthlyBalanceData(2021, 3, "42", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("368000"), BigDecimal("368000")),
    MonthlyBalanceData(2021, 3, "56", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("651660000"), BigDecimal("651660000"))
  )

  val fy2022MonthlyBalances: List[MonthlyBalanceData] = List(
    // 資産(借方残高)
    MonthlyBalanceData(2022, 3, "11", "", "", "", BigDecimal(0), BigDecimal("2777545000"), BigDecimal(0), BigDecimal("2777545000")),
    MonthlyBalanceData(2022, 3, "12", "", "", "", BigDecimal(0), BigDecimal("197354000"), BigDecimal(0), BigDecimal("197354000")),

    // 負債(貸方残高)
    MonthlyBalanceData(2022, 3, "21", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("640513000"), BigDecimal("640513000")),
    MonthlyBalanceData(2022, 3, "22", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("27153000"), BigDecimal("27153000")),

    // 純資産(貸方残高)
    MonthlyBalanceData(2022, 3, "31", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("100000000"), BigDecimal("100000000")),
    MonthlyBalanceData(2022, 3, "33", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("2207233000"), BigDecimal("2207233000")),

    // 費用(借方残高)
    MonthlyBalanceData(2022, 3, "51", "", "", "", BigDecimal(0), BigDecimal("1743821000"), BigDecimal(0), BigDecimal("1743821000")),
    MonthlyBalanceData(2022, 3, "52", "", "", "", BigDecimal(0), BigDecimal("2277050000"), BigDecimal(0), BigDecimal("2277050000")),
    MonthlyBalanceData(2022, 3, "53", "", "", "", BigDecimal(0), BigDecimal("1613000"), BigDecimal(0), BigDecimal("1613000")),
    MonthlyBalanceData(2022, 3, "55", "", "", "", BigDecimal(0), BigDecimal("169072000"), BigDecimal(0), BigDecimal("169072000")),

    // 収益(貸方残高)
    MonthlyBalanceData(2022, 3, "41", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("4547908000"), BigDecimal("4547908000")),
    MonthlyBalanceData(2022, 3, "42", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("11608000"), BigDecimal("11608000")),
    MonthlyBalanceData(2022, 3, "56", "", "", "", BigDecimal(0), BigDecimal(0), BigDecimal("367960000"), BigDecimal("367960000"))
  )
