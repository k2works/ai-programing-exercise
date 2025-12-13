use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

use crate::domain::financial::period_comparison::{PeriodComparisonReport, VarianceAnalysis};
use super::{BalanceSheetResponse, FinancialRatiosResponse, IncomeStatementResponse};

/// 期間比較レポートレスポンス
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct PeriodComparisonResponse {
    /// 当期（現在期間）
    pub current_period: NaiveDate,

    /// 前期（比較期間）
    pub previous_period: NaiveDate,

    /// 当期の貸借対照表
    pub current_bs: BalanceSheetResponse,

    /// 前期の貸借対照表
    pub previous_bs: BalanceSheetResponse,

    /// 当期の損益計算書
    pub current_pl: IncomeStatementResponse,

    /// 前期の損益計算書
    pub previous_pl: IncomeStatementResponse,

    /// 当期の財務指標
    pub current_ratios: FinancialRatiosResponse,

    /// 前期の財務指標
    pub previous_ratios: FinancialRatiosResponse,

    /// 増減分析
    pub variance_analysis: VarianceAnalysisResponse,
}

/// 増減分析レスポンス
#[derive(Debug, Clone, Serialize, Deserialize, ToSchema)]
pub struct VarianceAnalysisResponse {
    /// 売上高の増減額
    #[schema(value_type = String)]
    pub sales_variance: Decimal,

    /// 売上高の増減率（%）
    #[schema(value_type = String)]
    pub sales_variance_ratio: Decimal,

    /// 営業利益の増減額
    #[schema(value_type = String)]
    pub operating_profit_variance: Decimal,

    /// 営業利益の増減率（%）
    #[schema(value_type = String)]
    pub operating_profit_variance_ratio: Decimal,

    /// 総資産の増減額
    #[schema(value_type = String)]
    pub total_assets_variance: Decimal,

    /// 総資産の増減率（%）
    #[schema(value_type = String)]
    pub total_assets_variance_ratio: Decimal,

    /// 純資産の増減額
    #[schema(value_type = String)]
    pub total_equity_variance: Decimal,

    /// 純資産の増減率（%）
    #[schema(value_type = String)]
    pub total_equity_variance_ratio: Decimal,
}

impl From<PeriodComparisonReport> for PeriodComparisonResponse {
    fn from(report: PeriodComparisonReport) -> Self {
        Self {
            current_period: report.current_period,
            previous_period: report.previous_period,
            current_bs: BalanceSheetResponse::from(report.current_bs),
            previous_bs: BalanceSheetResponse::from(report.previous_bs),
            current_pl: IncomeStatementResponse::from(report.current_pl),
            previous_pl: IncomeStatementResponse::from(report.previous_pl),
            current_ratios: FinancialRatiosResponse::from(report.current_ratios),
            previous_ratios: FinancialRatiosResponse::from(report.previous_ratios),
            variance_analysis: VarianceAnalysisResponse::from(report.variance_analysis),
        }
    }
}

impl From<VarianceAnalysis> for VarianceAnalysisResponse {
    fn from(analysis: VarianceAnalysis) -> Self {
        Self {
            sales_variance: analysis.sales_variance,
            sales_variance_ratio: analysis.sales_variance_ratio,
            operating_profit_variance: analysis.operating_profit_variance,
            operating_profit_variance_ratio: analysis.operating_profit_variance_ratio,
            total_assets_variance: analysis.total_assets_variance,
            total_assets_variance_ratio: analysis.total_assets_variance_ratio,
            total_equity_variance: analysis.total_equity_variance,
            total_equity_variance_ratio: analysis.total_equity_variance_ratio,
        }
    }
}
