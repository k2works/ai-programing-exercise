// src/infrastructure/web/dto/FinancialStatementResponseDto.ts

/**
 * 貸借対照表項目 DTO
 */
export interface BalanceSheetItemDto {
  accountCode: string
  accountName: string
  amount: number
}

/**
 * 貸借対照表 DTO
 */
export interface BalanceSheetDto {
  asOfDate: string
  assets: BalanceSheetItemDto[]
  liabilities: BalanceSheetItemDto[]
  equity: BalanceSheetItemDto[]
  totalAssets: number
  totalLiabilities: number
  totalEquity: number
}

/**
 * 損益計算書項目 DTO
 */
export interface IncomeStatementItemDto {
  accountCode: string
  accountName: string
  amount: number
}

/**
 * 損益計算書 DTO
 */
export interface IncomeStatementDto {
  fromDate: string
  toDate: string
  revenues: IncomeStatementItemDto[]
  expenses: IncomeStatementItemDto[]
  totalRevenues: number
  totalExpenses: number
  grossProfit: number
  operatingIncome: number
  netIncome: number
}
