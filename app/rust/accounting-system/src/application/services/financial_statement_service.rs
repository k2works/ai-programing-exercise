use async_trait::async_trait;
use chrono::NaiveDate;
use sqlx::PgPool;

use crate::application::ports::input::financial_statement_usecase::FinancialStatementUseCase;
use crate::domain::financial::balance_sheet::BalanceSheet;
use crate::domain::financial::financial_ratios::FinancialRatios;
use crate::domain::financial::income_statement::IncomeStatement;

/// 財務諸表サービス（Application Service）
pub struct FinancialStatementService {
    pool: PgPool,
}

impl FinancialStatementService {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    // Note: The actual implementation methods are in the original
    // src/application/financial/financial_statement_service.rs
    // For now, we'll delegate to that implementation
}

#[async_trait]
impl FinancialStatementUseCase for FinancialStatementService {
    async fn generate_balance_sheet(
        &self,
        as_of_date: NaiveDate,
    ) -> Result<BalanceSheet, Box<dyn std::error::Error>> {
        // Use the implementation from the original service
        let original_service = crate::application::financial::financial_statement_service::FinancialStatementService::new(self.pool.clone());
        Ok(original_service.generate_balance_sheet(as_of_date).await?)
    }

    async fn generate_income_statement(
        &self,
        start_date: NaiveDate,
        end_date: NaiveDate,
    ) -> Result<IncomeStatement, Box<dyn std::error::Error>> {
        let original_service = crate::application::financial::financial_statement_service::FinancialStatementService::new(self.pool.clone());
        Ok(original_service
            .generate_income_statement(start_date, end_date)
            .await?)
    }

    async fn calculate_financial_ratios(
        &self,
        as_of_date: NaiveDate,
        period_start: NaiveDate,
        period_end: NaiveDate,
    ) -> Result<FinancialRatios, Box<dyn std::error::Error>> {
        let original_service = crate::application::financial::financial_statement_service::FinancialStatementService::new(self.pool.clone());
        let balance_sheet = original_service.generate_balance_sheet(as_of_date).await?;
        let income_statement = original_service
            .generate_income_statement(period_start, period_end)
            .await?;
        Ok(original_service.calculate_financial_ratios(&balance_sheet, &income_statement))
    }
}
