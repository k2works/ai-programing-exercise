use axum::{
    body::Body,
    extract::{Request, State},
    http::{Method, StatusCode},
    response::{IntoResponse, Response},
    routing::any,
    Router,
};
use tower_http::cors::CorsLayer;

#[derive(Clone)]
struct AppState {
    financial_service_url: String,
    management_service_url: String,
    http_client: reqwest::Client,
}

#[tokio::main]
async fn main() {
    // ログ初期化
    tracing_subscriber::fmt::init();

    // 環境変数読み込み
    dotenv::dotenv().ok();

    let financial_service_url = std::env::var("FINANCIAL_SERVICE_URL")
        .unwrap_or_else(|_| "http://localhost:8080".to_string());

    let management_service_url = std::env::var("MANAGEMENT_SERVICE_URL")
        .unwrap_or_else(|_| "http://localhost:8081".to_string());

    tracing::info!("Financial service URL: {}", financial_service_url);
    tracing::info!("Management service URL: {}", management_service_url);

    let state = AppState {
        financial_service_url,
        management_service_url,
        http_client: reqwest::Client::new(),
    };

    // ルーター設定
    let app = Router::new()
        .route("/api/v1/accounts", any(proxy_to_financial))
        .route("/api/v1/accounts/*path", any(proxy_to_financial))
        .route("/api/v1/journals", any(proxy_to_financial))
        .route("/api/v1/journals/*path", any(proxy_to_financial))
        .route("/api/v1/financial/*path", any(proxy_to_financial))
        .route("/api/v1/audit-logs", any(proxy_to_financial))
        .route("/api/v1/audit-logs/*path", any(proxy_to_financial))
        .route("/api/v1/analysis/*path", any(proxy_to_management))
        .route("/api/v1/comparison", any(proxy_to_management))
        .layer(CorsLayer::permissive())
        .with_state(state);

    // サーバー起動
    let listener = tokio::net::TcpListener::bind("0.0.0.0:8090")
        .await
        .unwrap();

    tracing::info!("API Gateway listening on port 8090");

    axum::serve(listener, app).await.unwrap();
}

async fn proxy_to_financial(
    State(state): State<AppState>,
    req: Request<Body>,
) -> Result<Response, StatusCode> {
    proxy_request(&state.financial_service_url, &state.http_client, req).await
}

async fn proxy_to_management(
    State(state): State<AppState>,
    req: Request<Body>,
) -> Result<Response, StatusCode> {
    proxy_request(&state.management_service_url, &state.http_client, req).await
}

async fn proxy_request(
    target_url: &str,
    client: &reqwest::Client,
    req: Request<Body>,
) -> Result<Response, StatusCode> {
    let method = req.method().clone();
    let uri = req.uri();
    let path_and_query = uri.path_and_query()
        .map(|pq| pq.as_str())
        .unwrap_or(uri.path());

    let target = format!("{}{}", target_url, path_and_query);

    tracing::debug!("Proxying {} {} to {}", method, path_and_query, target);

    // reqwestでリクエストを構築
    let mut request_builder = match method {
        Method::GET => client.get(&target),
        Method::POST => client.post(&target),
        Method::PUT => client.put(&target),
        Method::DELETE => client.delete(&target),
        Method::PATCH => client.patch(&target),
        _ => return Err(StatusCode::METHOD_NOT_ALLOWED),
    };

    // ヘッダーをコピー
    for (key, value) in req.headers() {
        if let Ok(value_str) = value.to_str() {
            request_builder = request_builder.header(key.as_str(), value_str);
        }
    }

    // リクエストを送信
    let response = request_builder
        .send()
        .await
        .map_err(|e| {
            tracing::error!("Proxy request failed: {}", e);
            StatusCode::BAD_GATEWAY
        })?;

    // レスポンスを構築
    let status = StatusCode::from_u16(response.status().as_u16())
        .unwrap_or(StatusCode::INTERNAL_SERVER_ERROR);

    let body = response
        .text()
        .await
        .map_err(|_| StatusCode::INTERNAL_SERVER_ERROR)?;

    Ok((status, body).into_response())
}
