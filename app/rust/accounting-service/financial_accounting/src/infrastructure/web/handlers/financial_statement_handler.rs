use axum::{
    extract::{Query, State},
    http::StatusCode,
    Json,
};
use chrono::NaiveDate;
use serde::Deserialize;
use std::sync::Arc;
use utoipa::{IntoParams, ToSchema};

use crate::application::ports::input::financial_statement_usecase::FinancialStatementUseCase;
use crate::application::financial::financial_statement_service::FinancialStatementService;
use crate::infrastructure::web::dtos::{
    BalanceSheetResponse, FinancialRatiosResponse, IncomeStatementResponse,
    PeriodComparisonResponse,
};

#[derive(Debug, Deserialize, IntoParams, ToSchema)]
pub struct BalanceSheetQuery {
    /// 基準日 (YYYY-MM-DD)
    pub as_of_date: NaiveDate,
}

#[derive(Debug, Deserialize, IntoParams, ToSchema)]
pub struct IncomeStatementQuery {
    /// 開始日 (YYYY-MM-DD)
    pub start_date: NaiveDate,
    /// 終了日 (YYYY-MM-DD)
    pub end_date: NaiveDate,
}

#[derive(Debug, Deserialize, IntoParams, ToSchema)]
pub struct FinancialRatiosQuery {
    /// 基準日 (YYYY-MM-DD)
    pub as_of_date: NaiveDate,
    /// 期間開始日 (YYYY-MM-DD)
    pub period_start: NaiveDate,
    /// 期間終了日 (YYYY-MM-DD)
    pub period_end: NaiveDate,
}

#[derive(Debug, Deserialize, IntoParams, ToSchema)]
pub struct PeriodComparisonQuery {
    /// 当期の期末日 (YYYY-MM-DD)
    pub current_period: NaiveDate,
    /// 前期の期末日 (YYYY-MM-DD)
    pub previous_period: NaiveDate,
}

/// 貸借対照表を取得
#[utoipa::path(
    get,
    path = "/api/v1/financial/balance-sheet",
    params(BalanceSheetQuery),
    responses(
        (status = 200, description = "貸借対照表の取得に成功", body = BalanceSheetResponse),
        (status = 500, description = "内部サーバーエラー")
    ),
    tag = "財務諸表"
)]
pub async fn get_balance_sheet(
    State(use_case): State<Arc<dyn FinancialStatementUseCase>>,
    Query(query): Query<BalanceSheetQuery>,
) -> Result<Json<BalanceSheetResponse>, (StatusCode, String)> {
    let balance_sheet = use_case
        .generate_balance_sheet(query.as_of_date)
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    Ok(Json(BalanceSheetResponse::from(balance_sheet)))
}

/// 損益計算書を取得
#[utoipa::path(
    get,
    path = "/api/v1/financial/income-statement",
    params(IncomeStatementQuery),
    responses(
        (status = 200, description = "損益計算書の取得に成功", body = IncomeStatementResponse),
        (status = 500, description = "内部サーバーエラー")
    ),
    tag = "財務諸表"
)]
pub async fn get_income_statement(
    State(use_case): State<Arc<dyn FinancialStatementUseCase>>,
    Query(query): Query<IncomeStatementQuery>,
) -> Result<Json<IncomeStatementResponse>, (StatusCode, String)> {
    let income_statement = use_case
        .generate_income_statement(query.start_date, query.end_date)
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    Ok(Json(IncomeStatementResponse::from(income_statement)))
}

/// 財務指標を取得
#[utoipa::path(
    get,
    path = "/api/v1/financial/ratios",
    params(FinancialRatiosQuery),
    responses(
        (status = 200, description = "財務指標の取得に成功", body = FinancialRatiosResponse),
        (status = 500, description = "内部サーバーエラー")
    ),
    tag = "財務諸表"
)]
pub async fn get_financial_ratios(
    State(use_case): State<Arc<dyn FinancialStatementUseCase>>,
    Query(query): Query<FinancialRatiosQuery>,
) -> Result<Json<FinancialRatiosResponse>, (StatusCode, String)> {
    let ratios = use_case
        .calculate_financial_ratios(query.as_of_date, query.period_start, query.period_end)
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    Ok(Json(FinancialRatiosResponse::from(ratios)))
}

/// 期間比較分析を取得
#[utoipa::path(
    get,
    path = "/api/v1/financial/comparison",
    params(PeriodComparisonQuery),
    responses(
        (status = 200, description = "期間比較分析の取得に成功", body = PeriodComparisonResponse),
        (status = 500, description = "内部サーバーエラー")
    ),
    tag = "財務諸表"
)]
pub async fn compare_periods(
    State(service): State<Arc<FinancialStatementService>>,
    Query(query): Query<PeriodComparisonQuery>,
) -> Result<Json<PeriodComparisonResponse>, (StatusCode, String)> {
    let report = service
        .compare_periods(query.current_period, query.previous_period)
        .await
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))?;

    Ok(Json(PeriodComparisonResponse::from(report)))
}
