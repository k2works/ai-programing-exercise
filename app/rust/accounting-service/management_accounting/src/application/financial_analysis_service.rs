use crate::domain::financial_data::{FinancialData, FinancialRatios};
use crate::infrastructure::adapters::financial_accounting_adapter::FinancialAccountingAdapter;
use shared::Result;

/// 財務分析サービス
pub struct FinancialAnalysisService {
    financial_adapter: FinancialAccountingAdapter,
}

impl FinancialAnalysisService {
    pub fn new(financial_adapter: FinancialAccountingAdapter) -> Self {
        Self { financial_adapter }
    }

    /// 指定された会計年度の財務比率を分析
    pub async fn analyze_fiscal_year(&self, fiscal_year: i32) -> Result<FinancialRatios> {
        // 財務会計サービスから財務データを取得
        let financial_data = self
            .financial_adapter
            .fetch_financial_data_by_fiscal_year(fiscal_year)
            .await?;

        // 財務比率を計算
        financial_data
            .calculate_ratios()
            .map_err(|e| shared::error::AppError::BusinessRule(e))
    }

    /// 2つの期間の財務データを比較
    pub async fn compare_periods(
        &self,
        current_year: i32,
        previous_year: i32,
    ) -> Result<PeriodComparison> {
        // 各期間の財務データを取得
        let current_data = self
            .financial_adapter
            .fetch_financial_data_by_fiscal_year(current_year)
            .await?;

        let previous_data = self
            .financial_adapter
            .fetch_financial_data_by_fiscal_year(previous_year)
            .await?;

        // 財務比率を計算
        let current_ratios = current_data
            .calculate_ratios()
            .map_err(|e| shared::error::AppError::BusinessRule(e))?;

        let previous_ratios = previous_data
            .calculate_ratios()
            .map_err(|e| shared::error::AppError::BusinessRule(e))?;

        Ok(PeriodComparison {
            current_year,
            previous_year,
            current_data,
            previous_data,
            current_ratios,
            previous_ratios,
        })
    }
}

/// 期間比較結果
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub struct PeriodComparison {
    pub current_year: i32,
    pub previous_year: i32,
    pub current_data: FinancialData,
    pub previous_data: FinancialData,
    pub current_ratios: FinancialRatios,
    pub previous_ratios: FinancialRatios,
}
