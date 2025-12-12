mod common;

use axum::{
    body::Body,
    extract::Request,
    http::StatusCode,
    routing::get,
    Router,
};
use serde_json::json;
use std::sync::Arc;
use tower::util::ServiceExt;

use accounting_system::application::ports::output::audit_log_repository::AuditLogRepository;
use accounting_system::domain::audit::audit_action::AuditAction;
use accounting_system::domain::audit::audit_log::AuditLog;
use accounting_system::infrastructure::persistence::repositories::audit_log_repository_impl::AuditLogRepositoryImpl;
use accounting_system::infrastructure::web::handlers::audit_log_handler;
use common::TestDatabase;

fn create_test_app(repository: Arc<dyn AuditLogRepository>) -> Router {
    Router::new()
        .route("/api/v1/audit-logs", get(audit_log_handler::get_all_audit_logs))
        .route(
            "/api/v1/audit-logs/entity/:entity_type/:entity_id",
            get(audit_log_handler::get_entity_audit_logs),
        )
        .route(
            "/api/v1/audit-logs/user/:user_id",
            get(audit_log_handler::get_user_audit_logs),
        )
        .with_state(repository)
}

async fn insert_test_audit_log(db: &TestDatabase, entity_type: &str, entity_id: &str, action: AuditAction, user_id: &str) {
    let log = AuditLog {
        id: None,
        entity_type: entity_type.to_string(),
        entity_id: entity_id.to_string(),
        action,
        user_id: user_id.to_string(),
        user_name: "Test User".to_string(),
        timestamp: chrono::Utc::now(),
        old_values: Some([("field1".to_string(), json!("old_value"))].iter().cloned().collect()),
        new_values: Some([("field1".to_string(), json!("new_value"))].iter().cloned().collect()),
        changes: Some([("field1".to_string(), json!("new_value"))].iter().cloned().collect()),
        reason: Some("Test reason".to_string()),
        ip_address: Some("127.0.0.1".to_string()),
        user_agent: Some("Test Agent".to_string()),
    };

    sqlx::query!(
        r#"
        INSERT INTO audit_log (
            entity_type, entity_id, action, user_id, user_name,
            timestamp, old_values, new_values, changes, reason, ip_address, user_agent
        ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
        "#,
        log.entity_type,
        log.entity_id,
        log.action.to_code(),
        log.user_id,
        log.user_name,
        log.timestamp,
        serde_json::to_value(&log.old_values).unwrap(),
        serde_json::to_value(&log.new_values).unwrap(),
        serde_json::to_value(&log.changes).unwrap(),
        log.reason,
        log.ip_address,
        log.user_agent,
    )
    .execute(&db.pool)
    .await
    .unwrap();
}

#[tokio::test]
async fn test_get_all_audit_logs() {
    let db = TestDatabase::new().await;

    // テストデータ挿入
    insert_test_audit_log(&db, "Account", "1001", AuditAction::Create, "user1").await;
    insert_test_audit_log(&db, "Journal", "JE-001", AuditAction::Update, "user2").await;

    let repository = Arc::new(AuditLogRepositoryImpl::new(db.pool.clone()));
    let app = create_test_app(repository);

    let request = Request::builder()
        .uri("/api/v1/audit-logs?limit=10&offset=0")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    assert_eq!(response.status(), StatusCode::OK);

    let body = axum::body::to_bytes(response.into_body(), usize::MAX)
        .await
        .unwrap();
    let logs: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert!(logs.is_array());
    assert!(logs.as_array().unwrap().len() >= 2);
}

#[tokio::test]
async fn test_get_entity_audit_logs() {
    let db = TestDatabase::new().await;

    // テストデータ挿入
    insert_test_audit_log(&db, "Account", "1001", AuditAction::Create, "user1").await;
    insert_test_audit_log(&db, "Account", "1001", AuditAction::Update, "user1").await;
    insert_test_audit_log(&db, "Account", "1002", AuditAction::Create, "user2").await;

    let repository = Arc::new(AuditLogRepositoryImpl::new(db.pool.clone()));
    let app = create_test_app(repository);

    let request = Request::builder()
        .uri("/api/v1/audit-logs/entity/Account/1001?limit=10&offset=0")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    assert_eq!(response.status(), StatusCode::OK);

    let body = axum::body::to_bytes(response.into_body(), usize::MAX)
        .await
        .unwrap();
    let logs: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert!(logs.is_array());
    let logs_array = logs.as_array().unwrap();
    assert_eq!(logs_array.len(), 2);

    // すべてのログが Account 1001 に関するものであることを確認
    for log in logs_array {
        assert_eq!(log["entity_type"], "Account");
        assert_eq!(log["entity_id"], "1001");
    }
}

#[tokio::test]
async fn test_get_user_audit_logs() {
    let db = TestDatabase::new().await;

    // テストデータ挿入
    insert_test_audit_log(&db, "Account", "1001", AuditAction::Create, "user1").await;
    insert_test_audit_log(&db, "Journal", "JE-001", AuditAction::Update, "user1").await;
    insert_test_audit_log(&db, "Account", "1002", AuditAction::Create, "user2").await;

    let repository = Arc::new(AuditLogRepositoryImpl::new(db.pool.clone()));
    let app = create_test_app(repository);

    let request = Request::builder()
        .uri("/api/v1/audit-logs/user/user1?limit=10&offset=0")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    assert_eq!(response.status(), StatusCode::OK);

    let body = axum::body::to_bytes(response.into_body(), usize::MAX)
        .await
        .unwrap();
    let logs: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert!(logs.is_array());
    let logs_array = logs.as_array().unwrap();
    assert_eq!(logs_array.len(), 2);

    // すべてのログが user1 によるものであることを確認
    for log in logs_array {
        assert_eq!(log["user_id"], "user1");
    }
}

#[tokio::test]
async fn test_pagination() {
    let db = TestDatabase::new().await;

    // 複数のテストデータを挿入
    for i in 1..=5 {
        insert_test_audit_log(&db, "Account", &format!("100{}", i), AuditAction::Create, "user1").await;
    }

    let repository = Arc::new(AuditLogRepositoryImpl::new(db.pool.clone()));
    let app = create_test_app(repository);

    // 最初の2件を取得
    let request = Request::builder()
        .uri("/api/v1/audit-logs?limit=2&offset=0")
        .body(Body::empty())
        .unwrap();

    let response = app.clone().oneshot(request).await.unwrap();
    assert_eq!(response.status(), StatusCode::OK);

    let body = axum::body::to_bytes(response.into_body(), usize::MAX)
        .await
        .unwrap();
    let logs: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert!(logs.is_array());
    assert_eq!(logs.as_array().unwrap().len(), 2);

    // 次の2件を取得
    let request = Request::builder()
        .uri("/api/v1/audit-logs?limit=2&offset=2")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();
    assert_eq!(response.status(), StatusCode::OK);

    let body = axum::body::to_bytes(response.into_body(), usize::MAX)
        .await
        .unwrap();
    let logs: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert!(logs.is_array());
    assert!(logs.as_array().unwrap().len() >= 2);
}

#[tokio::test]
async fn test_empty_result() {
    let db = TestDatabase::new().await;

    let repository = Arc::new(AuditLogRepositoryImpl::new(db.pool.clone()));
    let app = create_test_app(repository);

    let request = Request::builder()
        .uri("/api/v1/audit-logs/entity/Account/9999?limit=10&offset=0")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    assert_eq!(response.status(), StatusCode::OK);

    let body = axum::body::to_bytes(response.into_body(), usize::MAX)
        .await
        .unwrap();
    let logs: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert!(logs.is_array());
    assert_eq!(logs.as_array().unwrap().len(), 0);
}
