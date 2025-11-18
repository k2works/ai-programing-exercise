// src/application/financial-analysis/FinancialAnalysisUseCase.ts

/**
 * 財務分析のユースケースを定義するインターフェース（Input Port）
 */
export interface FinancialAnalysisUseCase {
  /**
   * 指定された会計年度の財務分析を実行
   */
  analyzeByFiscalYear(fiscalYear: number): Promise<FinancialAnalysisResult>

  /**
   * 複数期間の財務分析を比較
   */
  compareMultiplePeriods(fiscalYears: number[]): Promise<ComparativeAnalysisResult>
}

/**
 * 財務分析結果
 */
export interface FinancialAnalysisResult {
  fiscalYear: number
  financialData: {
    sales: number
    operatingProfit: number
    totalAssets: number
    tangibleFixedAssets: number
    currentAssets: number
    currentLiabilities: number
    quickAssets: number
    equity: number
  }
  ratios: {
    profitability: {
      operatingProfitMargin: number
    }
    efficiency: {
      totalAssetTurnover: number
      tangibleFixedAssetTurnover: number
    }
    safety: {
      currentRatio: number
      quickRatio: number
      equityRatio: number
    }
  }
}

/**
 * 比較分析結果
 */
export interface ComparativeAnalysisResult {
  periods: FinancialAnalysisResult[]
  trends: {
    profitability: {
      operatingProfitMarginChange: number
    }
    efficiency: {
      totalAssetTurnoverChange: number
    }
    safety: {
      equityRatioChange: number
    }
  }
}
