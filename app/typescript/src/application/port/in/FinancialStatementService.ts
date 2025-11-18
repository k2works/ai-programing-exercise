// src/application/port/in/FinancialStatementService.ts
import { PrismaClient, MonthlyAccountBalance, Account } from '@prisma/client'
import {
  FinancialStatementUseCase,
  BalanceSheet,
  BalanceSheetItem,
  IncomeStatement,
  IncomeStatementItem
} from './FinancialStatementUseCase'

type MonthlyBalanceWithAccount = MonthlyAccountBalance & {
  account: Account
}

/**
 * 財務諸表サービス（Application Service）
 * 残高データから財務諸表を生成
 */
export class FinancialStatementService implements FinancialStatementUseCase {
  constructor(private prisma: PrismaClient) {}

  async generateBalanceSheet(asOfDate: Date): Promise<BalanceSheet> {
    const fiscalYearMonth = this.toFiscalYearMonth(asOfDate)

    const balances = await this.prisma.monthlyAccountBalance.findMany({
      where: { fiscalYearMonth },
      include: { account: true }
    })

    const { assets, liabilities, equity } = this.classifyBalances(balances)

    return {
      asOfDate,
      assets,
      liabilities,
      equity,
      totalAssets: this.calculateTotal(assets),
      totalLiabilities: this.calculateTotal(liabilities),
      totalEquity: this.calculateTotal(equity)
    }
  }

  async generateIncomeStatement(fromDate: Date, toDate: Date): Promise<IncomeStatement> {
    const yearMonths = this.generateYearMonthRange(
      this.toFiscalYearMonth(fromDate),
      this.toFiscalYearMonth(toDate)
    )

    const accountTotals = await this.aggregateAccountTotals(yearMonths)
    const { revenues, expenses } = this.classifyIncomeStatementItems(accountTotals)

    const totalRevenues = this.calculateTotal(revenues)
    const totalExpenses = this.calculateTotal(expenses)
    const netIncome = totalRevenues - totalExpenses

    return {
      fromDate,
      toDate,
      revenues,
      expenses,
      totalRevenues,
      totalExpenses,
      grossProfit: netIncome,
      operatingIncome: netIncome,
      netIncome
    }
  }

  /**
   * 期間内の勘定科目残高を集計
   */
  private async aggregateAccountTotals(
    yearMonths: string[]
  ): Promise<Map<string, { name: string; type: string; total: number }>> {
    const accountTotals = new Map<string, { name: string; type: string; total: number }>()

    for (const yearMonth of yearMonths) {
      const balances = await this.prisma.monthlyAccountBalance.findMany({
        where: { fiscalYearMonth: yearMonth },
        include: { account: true }
      })

      this.updateAccountTotals(accountTotals, balances)
    }

    return accountTotals
  }

  /**
   * 勘定科目の累計を更新
   */
  private updateAccountTotals(
    accountTotals: Map<string, { name: string; type: string; total: number }>,
    balances: MonthlyBalanceWithAccount[]
  ): void {
    for (const balance of balances) {
      const key = balance.account.accountCode
      const existing = accountTotals.get(key) || {
        name: balance.account.accountName,
        type: balance.account.accountType,
        total: 0
      }

      const amount = Number(balance.debitAmount) - Number(balance.creditAmount)
      existing.total += amount

      accountTotals.set(key, existing)
    }
  }

  /**
   * 勘定科目を収益・費用に分類
   */
  private classifyIncomeStatementItems(
    accountTotals: Map<string, { name: string; type: string; total: number }>
  ): { revenues: IncomeStatementItem[]; expenses: IncomeStatementItem[] } {
    const revenues: IncomeStatementItem[] = []
    const expenses: IncomeStatementItem[] = []

    for (const [accountCode, data] of accountTotals.entries()) {
      if (data.total === 0) continue

      const item = this.createIncomeStatementItem(accountCode, data)
      this.categorizeIncomeItem(item, data.type, revenues, expenses)
    }

    return { revenues, expenses }
  }

  /**
   * 損益計算書項目を作成
   */
  private createIncomeStatementItem(
    accountCode: string,
    data: { name: string; total: number }
  ): IncomeStatementItem {
    return {
      accountCode,
      accountName: data.name,
      amount: Math.abs(data.total)
    }
  }

  /**
   * 損益計算書項目を分類
   */
  private categorizeIncomeItem(
    item: IncomeStatementItem,
    type: string,
    revenues: IncomeStatementItem[],
    expenses: IncomeStatementItem[]
  ): void {
    if (type === '収益') {
      revenues.push(item)
    } else if (type === '費用') {
      expenses.push(item)
    }
  }

  /**
   * 残高データを資産・負債・純資産に分類
   */
  private classifyBalances(balances: MonthlyBalanceWithAccount[]): {
    assets: BalanceSheetItem[]
    liabilities: BalanceSheetItem[]
    equity: BalanceSheetItem[]
  } {
    const assets: BalanceSheetItem[] = []
    const liabilities: BalanceSheetItem[] = []
    const equity: BalanceSheetItem[] = []

    for (const balance of balances) {
      const amount = Number(balance.closingBalance)
      if (amount === 0) continue

      const item: BalanceSheetItem = {
        accountCode: balance.account.accountCode,
        accountName: balance.account.accountName,
        amount
      }

      this.categorizeBalanceItem(item, balance.account.accountType, assets, liabilities, equity)
    }

    return { assets, liabilities, equity }
  }

  /**
   * 勘定科目種別に応じて項目を分類
   */
  private categorizeBalanceItem(
    item: BalanceSheetItem,
    accountType: string,
    assets: BalanceSheetItem[],
    liabilities: BalanceSheetItem[],
    equity: BalanceSheetItem[]
  ): void {
    switch (accountType) {
      case '資産':
        assets.push(item)
        break
      case '負債':
        liabilities.push(item)
        break
      case '純資産':
      case '資本':
        equity.push(item)
        break
    }
  }

  /**
   * 項目の合計を計算
   */
  private calculateTotal(items: BalanceSheetItem[]): number {
    return items.reduce((sum, item) => sum + item.amount, 0)
  }

  /**
   * 日付を会計年月（YYYYMM）に変換
   */
  private toFiscalYearMonth(date: Date): string {
    const year = date.getFullYear()
    const month = (date.getMonth() + 1).toString().padStart(2, '0')
    return `${year}${month}`
  }

  /**
   * 会計年月の範囲を生成
   */
  private generateYearMonthRange(fromYearMonth: string, toYearMonth: string): string[] {
    const result: string[] = []
    let current = fromYearMonth

    while (current <= toYearMonth) {
      result.push(current)

      // 次の月へ
      const year = parseInt(current.substring(0, 4))
      const month = parseInt(current.substring(4, 6))

      if (month === 12) {
        current = `${year + 1}01`
      } else {
        current = `${year}${(month + 1).toString().padStart(2, '0')}`
      }
    }

    return result
  }
}
