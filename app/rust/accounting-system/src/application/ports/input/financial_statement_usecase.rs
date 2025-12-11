use async_trait::async_trait;
use chrono::NaiveDate;

use crate::domain::financial::balance_sheet::BalanceSheet;
use crate::domain::financial::financial_ratios::FinancialRatios;
use crate::domain::financial::income_statement::IncomeStatement;

/// 財務諸表ユースケースのトレイト（Input Port）
#[async_trait]
pub trait FinancialStatementUseCase: Send + Sync {
    /// 貸借対照表を生成
    async fn generate_balance_sheet(
        &self,
        as_of_date: NaiveDate,
    ) -> Result<BalanceSheet, Box<dyn std::error::Error>>;

    /// 損益計算書を生成
    async fn generate_income_statement(
        &self,
        start_date: NaiveDate,
        end_date: NaiveDate,
    ) -> Result<IncomeStatement, Box<dyn std::error::Error>>;

    /// 財務指標を計算
    async fn calculate_financial_ratios(
        &self,
        as_of_date: NaiveDate,
        period_start: NaiveDate,
        period_end: NaiveDate,
    ) -> Result<FinancialRatios, Box<dyn std::error::Error>>;
}
