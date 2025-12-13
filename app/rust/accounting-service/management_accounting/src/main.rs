use axum::{
    extract::{Path, Query, State},
    http::StatusCode,
    response::Json,
    routing::get,
    Router,
};
use serde::Deserialize;
use std::sync::Arc;
use tower_http::cors::CorsLayer;

use management_accounting::application::financial_analysis_service::{
    FinancialAnalysisService, PeriodComparison,
};
use management_accounting::domain::financial_data::FinancialRatios;
use management_accounting::infrastructure::adapters::financial_accounting_adapter::FinancialAccountingAdapter;

#[derive(Clone)]
struct AppState {
    analysis_service: Arc<FinancialAnalysisService>,
}

#[tokio::main]
async fn main() {
    // ログ初期化
    tracing_subscriber::fmt::init();

    // 環境変数読み込み
    dotenv::dotenv().ok();

    let financial_service_url = std::env::var("FINANCIAL_SERVICE_URL")
        .unwrap_or_else(|_| "http://localhost:8080".to_string());

    tracing::info!("Financial service URL: {}", financial_service_url);

    // 腐敗防止層（ACL）の作成
    let financial_adapter = FinancialAccountingAdapter::new(financial_service_url);

    // 財務分析サービスの作成
    let analysis_service = Arc::new(FinancialAnalysisService::new(financial_adapter));

    let state = AppState { analysis_service };

    // ルーター設定
    let app = Router::new()
        .route("/health", get(health_check))
        .route("/api/v1/analysis/:fiscal_year", get(analyze_fiscal_year))
        .route("/api/v1/comparison", get(compare_periods))
        .layer(CorsLayer::permissive())
        .with_state(state);

    // サーバー起動
    let listener = tokio::net::TcpListener::bind("0.0.0.0:8081")
        .await
        .unwrap();

    tracing::info!("Management accounting service listening on port 8081");

    axum::serve(listener, app).await.unwrap();
}

async fn health_check() -> &'static str {
    "OK"
}

async fn analyze_fiscal_year(
    State(state): State<AppState>,
    Path(fiscal_year): Path<i32>,
) -> Result<Json<FinancialRatios>, (StatusCode, String)> {
    state
        .analysis_service
        .analyze_fiscal_year(fiscal_year)
        .await
        .map(Json)
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))
}

#[derive(Deserialize)]
struct ComparisonQuery {
    current_year: i32,
    previous_year: i32,
}

async fn compare_periods(
    State(state): State<AppState>,
    Query(query): Query<ComparisonQuery>,
) -> Result<Json<PeriodComparison>, (StatusCode, String)> {
    state
        .analysis_service
        .compare_periods(query.current_year, query.previous_year)
        .await
        .map(Json)
        .map_err(|e| (StatusCode::INTERNAL_SERVER_ERROR, e.to_string()))
}
