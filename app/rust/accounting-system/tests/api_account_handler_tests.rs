mod common;

use axum::{
    body::Body,
    http::{Request, StatusCode},
    Router,
};
use serde_json::json;
use std::sync::Arc;
use tower::util::ServiceExt;

use accounting_system::application::ports::input::account_usecase::AccountUseCase;
use accounting_system::application::services::account_service::AccountService;
use accounting_system::infrastructure::persistence::repositories::account_repository_impl::AccountRepositoryImpl;
use accounting_system::infrastructure::web::handlers::account_handler;

use common::TestDatabase;

/// テスト用のアプリケーションルーターを作成
fn create_test_app(use_case: Arc<dyn AccountUseCase>) -> Router {
    Router::new()
        .route(
            "/api/v1/accounts",
            axum::routing::get(account_handler::get_accounts)
                .post(account_handler::create_account),
        )
        .route(
            "/api/v1/accounts/:code",
            axum::routing::get(account_handler::get_account),
        )
        .with_state(use_case)
}

#[tokio::test]
async fn test_get_all_accounts() {
    // Given: テストデータベースと勘定科目
    let db = TestDatabase::new().await;

    // テストデータを挿入
    common::insert_account(&db.pool, "1110", "現金", "資産", "100000").await;
    common::insert_account(&db.pool, "2110", "買掛金", "負債", "50000").await;
    common::insert_account(&db.pool, "3110", "資本金", "純資産", "50000").await;

    // DIコンテナを構築
    let repository = Arc::new(AccountRepositoryImpl::new(db.pool.clone()));
    let service = AccountService::new(repository);
    let use_case: Arc<dyn AccountUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: GET /api/v1/accounts
    let request = Request::builder()
        .uri("/api/v1/accounts")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();
    let status = response.status();

    // デバッグ用: エラー時にボディを表示
    if status != StatusCode::OK {
        let body = axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap();
        let error_msg = String::from_utf8_lossy(&body);
        panic!("Expected status 200, got {}. Body: {}", status, error_msg);
    }

    // Then: ステータスコードが200
    assert_eq!(status, StatusCode::OK);

    // レスポンスボディを検証
    let body = axum::body::to_bytes(response.into_body(), usize::MAX)
        .await
        .unwrap();
    let accounts: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert!(accounts.is_array());
    let accounts_array = accounts.as_array().unwrap();
    assert_eq!(accounts_array.len(), 3);

    // 勘定科目コードの順序を確認
    assert_eq!(accounts_array[0]["account_code"], "1110");
    assert_eq!(accounts_array[1]["account_code"], "2110");
    assert_eq!(accounts_array[2]["account_code"], "3110");
}

#[tokio::test]
async fn test_get_account_by_code() {
    // Given: テストデータベースと特定の勘定科目
    let db = TestDatabase::new().await;

    common::insert_account(&db.pool, "1110", "現金", "資産", "100000").await;

    let repository = Arc::new(AccountRepositoryImpl::new(db.pool.clone()));
    let service = AccountService::new(repository);
    let use_case: Arc<dyn AccountUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: GET /api/v1/accounts/1110
    let request = Request::builder()
        .uri("/api/v1/accounts/1110")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Then: ステータスコードが200
    assert_eq!(response.status(), StatusCode::OK);

    // レスポンスボディを検証
    let body = axum::body::to_bytes(response.into_body(), usize::MAX)
        .await
        .unwrap();
    let account: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert_eq!(account["account_code"], "1110");
    assert_eq!(account["account_name"], "現金");
    assert_eq!(account["account_type"], "資産");
}

#[tokio::test]
async fn test_get_account_not_found() {
    // Given: テストデータベース（空）
    let db = TestDatabase::new().await;

    let repository = Arc::new(AccountRepositoryImpl::new(db.pool.clone()));
    let service = AccountService::new(repository);
    let use_case: Arc<dyn AccountUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: 存在しない勘定科目を取得
    let request = Request::builder()
        .uri("/api/v1/accounts/9999")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Then: ステータスコードが404
    assert_eq!(response.status(), StatusCode::NOT_FOUND);
}

#[tokio::test]
async fn test_create_account() {
    // Given: テストデータベース
    let db = TestDatabase::new().await;

    let repository = Arc::new(AccountRepositoryImpl::new(db.pool.clone()));
    let service = AccountService::new(repository);
    let use_case: Arc<dyn AccountUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: POST /api/v1/accounts
    let new_account = json!({
        "account_code": "1110",
        "account_name": "現金",
        "account_name_kana": "ゲンキン",
        "account_type": "資産",
        "is_summary_account": false,
        "bspl_type": "B",
        "transaction_element_type": "D",
        "expense_type": null,
        "display_order": 1,
        "is_aggregation_target": true,
        "tax_code": null
    });

    let request = Request::builder()
        .uri("/api/v1/accounts")
        .method("POST")
        .header("content-type", "application/json")
        .body(Body::from(serde_json::to_vec(&new_account).unwrap()))
        .unwrap();

    let response = app.oneshot(request).await.unwrap();
    let status = response.status();

    // デバッグ用: エラー時にボディを表示
    if status != StatusCode::CREATED {
        let body = axum::body::to_bytes(response.into_body(), usize::MAX)
            .await
            .unwrap();
        let error_msg = String::from_utf8_lossy(&body);
        panic!("Expected status 201, got {}. Body: {}", status, error_msg);
    }

    // Then: ステータスコードが201
    assert_eq!(status, StatusCode::CREATED);

    // レスポンスボディを検証
    let body = axum::body::to_bytes(response.into_body(), usize::MAX)
        .await
        .unwrap();
    let account: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert_eq!(account["account_code"], "1110");
    assert_eq!(account["account_name"], "現金");
    assert_eq!(account["account_type"], "資産");
}

#[tokio::test]
async fn test_create_account_with_empty_code() {
    // Given: テストデータベース
    let db = TestDatabase::new().await;

    let repository = Arc::new(AccountRepositoryImpl::new(db.pool.clone()));
    let service = AccountService::new(repository);
    let use_case: Arc<dyn AccountUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: 勘定科目コードが空の状態でPOST
    let invalid_account = json!({
        "account_code": "",
        "account_name": "現金",
        "account_type": "資産",
        "is_summary_account": false
    });

    let request = Request::builder()
        .uri("/api/v1/accounts")
        .method("POST")
        .header("content-type", "application/json")
        .body(Body::from(serde_json::to_vec(&invalid_account).unwrap()))
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Then: ステータスコードが400
    assert_eq!(response.status(), StatusCode::BAD_REQUEST);
}

#[tokio::test]
async fn test_create_duplicate_account() {
    // Given: テストデータベースに既存の勘定科目
    let db = TestDatabase::new().await;

    common::insert_account(&db.pool, "1110", "現金", "資産", "0").await;

    let repository = Arc::new(AccountRepositoryImpl::new(db.pool.clone()));
    let service = AccountService::new(repository);
    let use_case: Arc<dyn AccountUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: 同じ勘定科目コードで作成を試みる
    let duplicate_account = json!({
        "account_code": "1110",
        "account_name": "現金2",
        "account_name_kana": "ゲンキン2",
        "account_type": "資産",
        "is_summary_account": false,
        "bspl_type": "B",
        "transaction_element_type": "D",
        "expense_type": null,
        "display_order": 1,
        "is_aggregation_target": true,
        "tax_code": null
    });

    let request = Request::builder()
        .uri("/api/v1/accounts")
        .method("POST")
        .header("content-type", "application/json")
        .body(Body::from(
            serde_json::to_vec(&duplicate_account).unwrap(),
        ))
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Then: ステータスコードが400（重複エラー）
    assert_eq!(response.status(), StatusCode::BAD_REQUEST);
}
