//! 販売管理APIサーバー
//! Axumフレームワークを使用したREST API

use axum::{
    routing::{delete, get, post, put},
    Router,
};
use sales_management_db::handler::product;
use sales_management_db::service::ProductService;
use sqlx::PgPool;
use std::sync::Arc;
use tower_http::cors::CorsLayer;
use tower_http::trace::TraceLayer;
use utoipa::OpenApi;
use utoipa_swagger_ui::SwaggerUi;

#[derive(OpenApi)]
#[openapi(
    paths(
        product::create_product,
        product::get_all_products,
        product::get_product_by_id,
        product::update_product,
        product::delete_product,
    ),
    components(
        schemas(
            sales_management_db::dto::product::CreateProductRequest,
            sales_management_db::dto::product::UpdateProductRequest,
            sales_management_db::dto::product::ProductResponse,
        )
    ),
    tags(
        (name = "products", description = "商品管理 API")
    )
)]
struct ApiDoc;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // ロギング初期化
    tracing_subscriber::fmt::init();

    // 環境変数読み込み
    dotenv::dotenv().ok();

    // データベース接続
    let database_url = std::env::var("DATABASE_URL").expect("DATABASE_URL must be set");
    let pool = PgPool::connect(&database_url).await?;

    // マイグレーション実行
    tracing::info!("Running migrations...");
    sqlx::migrate!("./migrations").run(&pool).await?;
    tracing::info!("Migrations completed");

    // サービス層の初期化
    let product_service = Arc::new(ProductService::new(pool.clone()));

    // ルーター構築
    let app = Router::new()
        // ルートエンドポイント
        .route("/", get(root_handler))
        .route("/health", get(health_handler))
        // 商品API
        .route("/products", post(product::create_product))
        .route("/products", get(product::get_all_products))
        .route("/products/:prod_code", get(product::get_product_by_id))
        .route("/products/:prod_code", put(product::update_product))
        .route("/products/:prod_code", delete(product::delete_product))
        // Swagger UI
        .merge(SwaggerUi::new("/docs").url("/api-docs/openapi.json", ApiDoc::openapi()))
        // State
        .with_state(product_service)
        // ミドルウェア
        .layer(CorsLayer::permissive())
        .layer(TraceLayer::new_for_http());

    // サーバー起動
    let listener = tokio::net::TcpListener::bind("0.0.0.0:3000").await?;
    tracing::info!("🚀 Server is running on http://localhost:3000");
    tracing::info!("📚 Swagger UI is available at http://localhost:3000/docs");

    axum::serve(listener, app).await?;

    Ok(())
}

async fn root_handler() -> axum::Json<serde_json::Value> {
    axum::Json(serde_json::json!({
        "message": "Sales Management API",
        "version": "1.0.0",
        "endpoints": ["/products", "/customers", "/suppliers", "/stocks"],
        "docs": "/docs"
    }))
}

async fn health_handler() -> axum::Json<serde_json::Value> {
    axum::Json(serde_json::json!({
        "status": "ok",
        "timestamp": chrono::Utc::now().to_rfc3339()
    }))
}
