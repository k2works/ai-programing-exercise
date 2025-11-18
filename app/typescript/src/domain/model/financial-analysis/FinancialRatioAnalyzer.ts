// src/domain/financial-analysis/FinancialRatioAnalyzer.ts
import { FinancialData } from './FinancialData'

/**
 * 財務分析結果を表す型
 */
export interface AnalysisResult {
  profitability: {
    operatingProfitMargin: number // 売上高営業利益率
  }
  efficiency: {
    totalAssetTurnover: number // 総資本回転率
    tangibleFixedAssetTurnover: number // 有形固定資産回転率
  }
  safety: {
    currentRatio: number // 流動比率
    quickRatio: number // 当座比率
    equityRatio: number // 自己資本比率
  }
}

/**
 * 財務分析指標を計算するクラス
 */
export class FinancialRatioAnalyzer {
  /**
   * 財務データを分析して各種指標を計算
   */
  analyze(data: FinancialData): AnalysisResult {
    return {
      profitability: {
        operatingProfitMargin: this.calculateOperatingProfitMargin(data.sales, data.operatingProfit)
      },
      efficiency: {
        totalAssetTurnover: this.calculateTotalAssetTurnover(data.sales, data.totalAssets),
        tangibleFixedAssetTurnover: this.calculateTangibleFixedAssetTurnover(
          data.sales,
          data.tangibleFixedAssets
        )
      },
      safety: {
        currentRatio: this.calculateCurrentRatio(data.currentAssets, data.currentLiabilities),
        quickRatio: this.calculateQuickRatio(data.quickAssets, data.currentLiabilities),
        equityRatio: this.calculateEquityRatio(data.equity, data.totalAssets)
      }
    }
  }

  private calculateOperatingProfitMargin(sales: number, operatingProfit: number): number {
    if (sales === 0) throw new Error('売上高がゼロのため計算できません')
    return (operatingProfit / sales) * 100
  }

  private calculateTotalAssetTurnover(sales: number, totalAssets: number): number {
    if (totalAssets === 0) throw new Error('総資本がゼロのため計算できません')
    return sales / totalAssets
  }

  private calculateTangibleFixedAssetTurnover(sales: number, tangibleFixedAssets: number): number {
    if (tangibleFixedAssets === 0) throw new Error('有形固定資産がゼロのため計算できません')
    return sales / tangibleFixedAssets
  }

  private calculateCurrentRatio(currentAssets: number, currentLiabilities: number): number {
    if (currentLiabilities === 0) throw new Error('流動負債がゼロのため計算できません')
    return (currentAssets / currentLiabilities) * 100
  }

  private calculateQuickRatio(quickAssets: number, currentLiabilities: number): number {
    if (currentLiabilities === 0) throw new Error('流動負債がゼロのため計算できません')
    return (quickAssets / currentLiabilities) * 100
  }

  private calculateEquityRatio(equity: number, totalAssets: number): number {
    if (totalAssets === 0) throw new Error('総資本がゼロのため計算できません')
    return (equity / totalAssets) * 100
  }
}
