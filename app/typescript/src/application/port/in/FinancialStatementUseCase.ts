// src/application/port/in/FinancialStatementUseCase.ts

/**
 * 貸借対照表項目
 */
export interface BalanceSheetItem {
  accountCode: string
  accountName: string
  amount: number
}

/**
 * 貸借対照表
 */
export interface BalanceSheet {
  asOfDate: Date
  assets: BalanceSheetItem[]
  liabilities: BalanceSheetItem[]
  equity: BalanceSheetItem[]
  totalAssets: number
  totalLiabilities: number
  totalEquity: number
}

/**
 * 損益計算書項目
 */
export interface IncomeStatementItem {
  accountCode: string
  accountName: string
  amount: number
}

/**
 * 損益計算書
 */
export interface IncomeStatement {
  fromDate: Date
  toDate: Date
  revenues: IncomeStatementItem[]
  expenses: IncomeStatementItem[]
  totalRevenues: number
  totalExpenses: number
  grossProfit: number
  operatingIncome: number
  netIncome: number
}

/**
 * 財務諸表ユースケース（Input Port）
 * 財務諸表の生成と財務分析のインターフェース定義
 */
export interface FinancialStatementUseCase {
  /**
   * 貸借対照表を生成する
   * @param asOfDate 基準日
   * @returns 貸借対照表
   */
  generateBalanceSheet(asOfDate: Date): Promise<BalanceSheet>

  /**
   * 損益計算書を生成する
   * @param fromDate 開始日
   * @param toDate 終了日
   * @returns 損益計算書
   */
  generateIncomeStatement(fromDate: Date, toDate: Date): Promise<IncomeStatement>
}
