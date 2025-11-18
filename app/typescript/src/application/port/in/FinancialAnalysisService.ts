// src/application/port/in/FinancialAnalysisService.ts

import type { JournalRepository } from '../out/JournalRepository'
import { FinancialData } from '../../../domain/model/financial-analysis/FinancialData'
import { FinancialRatioAnalyzer } from '../../../domain/model/financial-analysis/FinancialRatioAnalyzer'
import type {
  FinancialAnalysisUseCase,
  FinancialAnalysisResult,
  ComparativeAnalysisResult
} from './FinancialAnalysisUseCase'

export class FinancialAnalysisService implements FinancialAnalysisUseCase {
  private readonly analyzer: FinancialRatioAnalyzer

  constructor(private readonly journalRepository: JournalRepository) {
    this.analyzer = new FinancialRatioAnalyzer()
  }

  async analyzeByFiscalYear(fiscalYear: number): Promise<FinancialAnalysisResult> {
    // 会計年度の開始日と終了日を計算（日本の会計年度：4月1日〜翌年3月31日）
    const startDate = new Date(fiscalYear - 1, 3, 1) // 4月1日（月は0始まり）
    const endDate = new Date(fiscalYear, 2, 31) // 3月31日

    // 指定された会計年度の仕訳を取得
    const journals = await this.journalRepository.findByDateRange(startDate, endDate)

    if (journals.length === 0) {
      throw new Error(`会計年度 ${fiscalYear} の仕訳データが見つかりません`)
    }

    // 仕訳明細から勘定科目別の金額を集計
    const entries = journals.flatMap((journal) =>
      journal.details.flatMap((detail) =>
        detail.items.map((item) => ({
          accountCode: item.accountCode,
          debitAmount: item.debitOrCredit === '借方' ? item.amount : 0,
          creditAmount: item.debitOrCredit === '貸方' ? item.amount : 0
        }))
      )
    )

    // 勘定科目ごとに集計
    const accountMap = new Map<string, { debitAmount: number; creditAmount: number }>()
    for (const entry of entries) {
      const existing = accountMap.get(entry.accountCode) || { debitAmount: 0, creditAmount: 0 }
      existing.debitAmount += entry.debitAmount
      existing.creditAmount += entry.creditAmount
      accountMap.set(entry.accountCode, existing)
    }

    // Map を配列に変換
    const aggregatedEntries = Array.from(accountMap.entries()).map(([accountCode, amounts]) => ({
      accountCode,
      debitAmount: amounts.debitAmount,
      creditAmount: amounts.creditAmount
    }))

    // 財務データを生成
    const financialData = FinancialData.fromJournalEntries(fiscalYear, aggregatedEntries)

    // 財務分析を実行
    const ratios = this.analyzer.analyze(financialData)

    return {
      fiscalYear,
      financialData: {
        sales: financialData.sales,
        operatingProfit: financialData.operatingProfit,
        totalAssets: financialData.totalAssets,
        tangibleFixedAssets: financialData.tangibleFixedAssets,
        currentAssets: financialData.currentAssets,
        currentLiabilities: financialData.currentLiabilities,
        quickAssets: financialData.quickAssets,
        equity: financialData.equity
      },
      ratios
    }
  }

  async compareMultiplePeriods(fiscalYears: number[]): Promise<ComparativeAnalysisResult> {
    // 各期間の分析を実行
    const periods = await Promise.all(
      fiscalYears.map((fiscalYear) => this.analyzeByFiscalYear(fiscalYear))
    )

    // 最初の期間と最後の期間の差分を計算
    const first = periods[0]
    const last = periods[periods.length - 1]

    const trends = {
      profitability: {
        operatingProfitMarginChange:
          last.ratios.profitability.operatingProfitMargin -
          first.ratios.profitability.operatingProfitMargin
      },
      efficiency: {
        totalAssetTurnoverChange:
          last.ratios.efficiency.totalAssetTurnover - first.ratios.efficiency.totalAssetTurnover
      },
      safety: {
        equityRatioChange: last.ratios.safety.equityRatio - first.ratios.safety.equityRatio
      }
    }

    return {
      periods,
      trends
    }
  }
}
