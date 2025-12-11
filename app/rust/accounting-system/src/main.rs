use axum::{routing::get, Router};
use sqlx::PgPool;
use std::sync::Arc;
use tower_http::trace::TraceLayer;
use utoipa::OpenApi;
use utoipa_swagger_ui::SwaggerUi;

use accounting_system::application::ports::input::account_usecase::AccountUseCase;
use accounting_system::application::ports::input::financial_statement_usecase::FinancialStatementUseCase;
use accounting_system::application::ports::input::journal_usecase::JournalUseCase;
use accounting_system::application::services::account_service::AccountService;
use accounting_system::application::services::financial_statement_service::FinancialStatementService;
use accounting_system::application::services::journal_service::JournalService;
use accounting_system::infrastructure::persistence::repositories::account_repository_impl::AccountRepositoryImpl;
use accounting_system::infrastructure::persistence::repositories::journal_repository_impl::JournalRepositoryImpl;
use accounting_system::infrastructure::web::dtos::{
    AccountRequest, AccountResponse, BalanceSheetResponse, ErrorResponse, FinancialRatiosResponse,
    IncomeStatementResponse, JournalRequest, JournalResponse,
};
use accounting_system::infrastructure::web::handlers::{
    account_handler, financial_statement_handler, journal_handler,
};

#[derive(OpenApi)]
#[openapi(
    paths(
        account_handler::get_accounts,
        account_handler::get_account,
        account_handler::create_account,
        journal_handler::get_journals,
        journal_handler::get_journal,
        journal_handler::create_journal,
        financial_statement_handler::get_balance_sheet,
        financial_statement_handler::get_income_statement,
        financial_statement_handler::get_financial_ratios,
    ),
    components(
        schemas(
            AccountRequest,
            AccountResponse,
            JournalRequest,
            JournalResponse,
            ErrorResponse,
            BalanceSheetResponse,
            IncomeStatementResponse,
            FinancialRatiosResponse,
            financial_statement_handler::BalanceSheetQuery,
            financial_statement_handler::IncomeStatementQuery,
            financial_statement_handler::FinancialRatiosQuery,
        )
    ),
    tags(
        (name = "勘定科目", description = "勘定科目マスタの管理"),
        (name = "仕訳", description = "仕訳の管理"),
        (name = "財務諸表", description = "財務諸表の生成と取得")
    ),
    info(
        title = "会計システム API",
        version = "1.0.0",
        description = "Rust + SQLx + Axum による会計システムの RESTful API",
        contact(
            name = "API Support",
        )
    )
)]
struct ApiDoc;

#[tokio::main]
async fn main() {
    // ロギング初期化
    tracing_subscriber::fmt::init();

    // 環境変数から DATABASE_URL を読み込む
    dotenv::dotenv().ok();
    let database_url = std::env::var("DATABASE_URL").expect("DATABASE_URL must be set");

    // データベース接続
    let pool = PgPool::connect(&database_url)
        .await
        .expect("Failed to connect to database");

    // 依存性の組み立て（Dependency Injection）
    // Account API
    let repository = Arc::new(AccountRepositoryImpl::new(pool.clone()));
    let account_service = AccountService::new(repository);
    let account_usecase: Arc<dyn AccountUseCase> = Arc::new(account_service);

    // Journal API
    let journal_repository = Arc::new(JournalRepositoryImpl::new(pool.clone()));
    let journal_service = JournalService::new(journal_repository);
    let journal_usecase: Arc<dyn JournalUseCase> = Arc::new(journal_service);

    // Financial Statement API
    let financial_service = FinancialStatementService::new(pool.clone());
    let financial_usecase: Arc<dyn FinancialStatementUseCase> = Arc::new(financial_service);

    // ルーティング
    // Account API のルーター
    let account_router = Router::new()
        .route(
            "/api/v1/accounts",
            get(account_handler::get_accounts).post(account_handler::create_account),
        )
        .route("/api/v1/accounts/:code", get(account_handler::get_account))
        .with_state(account_usecase);

    // Journal API のルーター
    let journal_router = Router::new()
        .route(
            "/api/v1/journals",
            get(journal_handler::get_journals).post(journal_handler::create_journal),
        )
        .route(
            "/api/v1/journals/:journal_no",
            get(journal_handler::get_journal),
        )
        .with_state(journal_usecase);

    // Financial Statement API のルーター
    let financial_router = Router::new()
        .route(
            "/api/v1/financial/balance-sheet",
            get(financial_statement_handler::get_balance_sheet),
        )
        .route(
            "/api/v1/financial/income-statement",
            get(financial_statement_handler::get_income_statement),
        )
        .route(
            "/api/v1/financial/ratios",
            get(financial_statement_handler::get_financial_ratios),
        )
        .with_state(financial_usecase);

    // ルーターを結合
    let app = account_router
        .merge(journal_router)
        .merge(financial_router)
        .merge(SwaggerUi::new("/swagger-ui").url("/api-docs/openapi.json", ApiDoc::openapi()))
        .layer(TraceLayer::new_for_http());

    // サーバー起動
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await.unwrap();
    let addr = listener.local_addr().unwrap();

    tracing::info!("🚀 Server listening on {}", addr);
    tracing::info!("📚 Swagger UI: http://{}/swagger-ui", addr);
    tracing::info!("📄 OpenAPI JSON: http://{}/api-docs/openapi.json", addr);

    axum::serve(listener, app).await.unwrap();
}
