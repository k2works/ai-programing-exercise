use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

use super::balance_sheet::BalanceSheet;
use super::financial_ratios::FinancialRatios;
use super::income_statement::IncomeStatement;

/// 期間比較レポート
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PeriodComparisonReport {
    /// 当期（現在期間）
    pub current_period: NaiveDate,

    /// 前期（比較期間）
    pub previous_period: NaiveDate,

    /// 当期の貸借対照表
    pub current_bs: BalanceSheet,

    /// 前期の貸借対照表
    pub previous_bs: BalanceSheet,

    /// 当期の損益計算書
    pub current_pl: IncomeStatement,

    /// 前期の損益計算書
    pub previous_pl: IncomeStatement,

    /// 当期の財務指標
    pub current_ratios: FinancialRatios,

    /// 前期の財務指標
    pub previous_ratios: FinancialRatios,

    /// 増減分析
    pub variance_analysis: VarianceAnalysis,
}

/// 増減分析
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VarianceAnalysis {
    /// 売上高の増減額
    pub sales_variance: Decimal,

    /// 売上高の増減率（%）
    pub sales_variance_ratio: Decimal,

    /// 営業利益の増減額
    pub operating_profit_variance: Decimal,

    /// 営業利益の増減率（%）
    pub operating_profit_variance_ratio: Decimal,

    /// 総資産の増減額
    pub total_assets_variance: Decimal,

    /// 総資産の増減率（%）
    pub total_assets_variance_ratio: Decimal,

    /// 純資産の増減額
    pub total_equity_variance: Decimal,

    /// 純資産の増減率（%）
    pub total_equity_variance_ratio: Decimal,
}
