// src/domain/financial-analysis/FinancialData.ts

/**
 * 財務データの値オブジェクト
 */
export class FinancialData {
  constructor(
    public readonly fiscalYear: number,
    public readonly sales: number,
    public readonly operatingProfit: number,
    public readonly totalAssets: number,
    public readonly tangibleFixedAssets: number,
    public readonly currentAssets: number,
    public readonly currentLiabilities: number,
    public readonly quickAssets: number,
    public readonly equity: number
  ) {
    Object.freeze(this)
  }

  /**
   * 仕訳データから財務データを生成
   */
  static fromJournalEntries(
    fiscalYear: number,
    entries: Array<{ accountCode: string; debitAmount: number; creditAmount: number }>
  ): FinancialData {
    // 各勘定科目の金額を集計
    const getAmount = (code: string): number => {
      const entry = entries.find((e) => e.accountCode === code)
      if (!entry) return 0
      return entry.debitAmount > 0 ? entry.debitAmount : entry.creditAmount
    }

    const sales = getAmount('41') // 売上高
    const operatingProfit = getAmount('41') - getAmount('51') - getAmount('52') // 営業利益
    const totalAssets = getAmount('11') + getAmount('12') // 総資産
    const tangibleFixedAssets = getAmount('121') // 有形固定資産
    const currentAssets = getAmount('11') // 流動資産
    const currentLiabilities = getAmount('21') // 流動負債
    const quickAssets = currentAssets - getAmount('114') // 当座資産（流動資産 - 棚卸資産）
    const equity = getAmount('31') + getAmount('33') // 純資産

    return new FinancialData(
      fiscalYear,
      sales,
      operatingProfit,
      totalAssets,
      tangibleFixedAssets,
      currentAssets,
      currentLiabilities,
      quickAssets,
      equity
    )
  }
}
