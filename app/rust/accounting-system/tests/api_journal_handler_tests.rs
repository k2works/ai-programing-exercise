mod common;

use axum::{
    body::Body,
    http::{Request, StatusCode},
    Router,
};
use chrono::NaiveDate;
use serde_json::json;
use sqlx::PgPool;
use std::sync::Arc;
use tower::util::ServiceExt;

use accounting_system::application::ports::input::journal_usecase::JournalUseCase;
use accounting_system::application::services::journal_service::JournalService;
use accounting_system::infrastructure::persistence::repositories::journal_repository_impl::JournalRepositoryImpl;
use accounting_system::infrastructure::web::handlers::journal_handler;

use common::TestDatabase;

/// テスト用のアプリケーションルーターを作成
fn create_test_app(use_case: Arc<dyn JournalUseCase>) -> Router {
    Router::new()
        .route(
            "/api/v1/journals",
            axum::routing::get(journal_handler::get_journals)
                .post(journal_handler::create_journal),
        )
        .route(
            "/api/v1/journals/:journal_no",
            axum::routing::get(journal_handler::get_journal),
        )
        .with_state(use_case)
}

/// テスト用勘定科目を登録
async fn insert_test_accounts(pool: &PgPool) {
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
        VALUES
        ('1010', '現金', '資産', false, true, 0),
        ('4100', '売上', '収益', false, true, 0),
        ('5110', '仕入高', '費用', false, true, 0)
        ON CONFLICT ("勘定科目コード") DO NOTHING
        "#,
    )
    .execute(pool)
    .await
    .unwrap();
}

/// テスト用仕訳を登録
async fn insert_test_journal(pool: &PgPool, journal_no: &str, date: NaiveDate, amount: i64) {
    // トランザクションを開始
    let mut tx = pool.begin().await.unwrap();

    // 仕訳ヘッダー
    sqlx::query(
        r#"
        INSERT INTO "仕訳" (
            "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
            "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES ($1, $2, $2, 0, 1, 1, 0, 0)
        "#,
    )
    .bind(journal_no)
    .bind(date)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 仕訳明細
    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" ("仕訳伝票番号", "仕訳行番号", "行摘要")
        VALUES ($1, 1, 'テスト仕訳')
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 借方貸方明細（借方）
    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分", "通貨コード", "為替レート",
            "勘定科目コード", "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 1, 'D', 'JPY', 1.0, '1010', $2, $2, 0)
        "#,
    )
    .bind(journal_no)
    .bind(amount)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 借方貸方明細（貸方）
    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分", "通貨コード", "為替レート",
            "勘定科目コード", "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 1, 'C', 'JPY', 1.0, '4100', $2, $2, 0)
        "#,
    )
    .bind(journal_no)
    .bind(amount)
    .execute(&mut *tx)
    .await
    .unwrap();

    // トランザクションをコミット
    tx.commit().await.unwrap();
}

#[tokio::test]
async fn test_get_all_journals() {
    // Given: テストデータベースと仕訳データ
    let db = TestDatabase::new().await;
    insert_test_accounts(&db.pool).await;

    insert_test_journal(
        &db.pool,
        "JE-001",
        NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
        100000,
    )
    .await;
    insert_test_journal(
        &db.pool,
        "JE-002",
        NaiveDate::from_ymd_opt(2025, 1, 2).unwrap(),
        200000,
    )
    .await;

    // DIコンテナを構築
    let repository = Arc::new(JournalRepositoryImpl::new(db.pool.clone()));
    let service = JournalService::new(repository);
    let use_case: Arc<dyn JournalUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: GET /api/v1/journals
    let request = Request::builder()
        .uri("/api/v1/journals")
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
    let journals: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert!(journals.is_array());
    let journals_array = journals.as_array().unwrap();
    assert_eq!(journals_array.len(), 2);

    // 仕訳番号の順序を確認
    assert_eq!(journals_array[0]["journal_no"], "JE-001");
    assert_eq!(journals_array[1]["journal_no"], "JE-002");
}

#[tokio::test]
async fn test_get_journal_by_no() {
    // Given: テストデータベースと特定の仕訳
    let db = TestDatabase::new().await;
    insert_test_accounts(&db.pool).await;

    insert_test_journal(
        &db.pool,
        "JE-TEST-001",
        NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
        100000,
    )
    .await;

    let repository = Arc::new(JournalRepositoryImpl::new(db.pool.clone()));
    let service = JournalService::new(repository);
    let use_case: Arc<dyn JournalUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: GET /api/v1/journals/JE-TEST-001
    let request = Request::builder()
        .uri("/api/v1/journals/JE-TEST-001")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Then: ステータスコードが200
    assert_eq!(response.status(), StatusCode::OK);

    // レスポンスボディを検証
    let body = axum::body::to_bytes(response.into_body(), usize::MAX)
        .await
        .unwrap();
    let journal: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert_eq!(journal["journal_no"], "JE-TEST-001");
    assert_eq!(journal["journal_date"], "2025-01-01");

    // 明細を確認
    let details = journal["details"].as_array().unwrap();
    assert!(details.len() > 0);
}

#[tokio::test]
async fn test_get_journal_not_found() {
    // Given: テストデータベース（空）
    let db = TestDatabase::new().await;
    insert_test_accounts(&db.pool).await;

    let repository = Arc::new(JournalRepositoryImpl::new(db.pool.clone()));
    let service = JournalService::new(repository);
    let use_case: Arc<dyn JournalUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: 存在しない仕訳を取得
    let request = Request::builder()
        .uri("/api/v1/journals/NOT-EXISTS")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Then: ステータスコードが404
    assert_eq!(response.status(), StatusCode::NOT_FOUND);
}

#[tokio::test]
async fn test_create_journal() {
    // Given: テストデータベース
    let db = TestDatabase::new().await;
    insert_test_accounts(&db.pool).await;

    let repository = Arc::new(JournalRepositoryImpl::new(db.pool.clone()));
    let service = JournalService::new(repository);
    let use_case: Arc<dyn JournalUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: POST /api/v1/journals
    let new_journal = json!({
        "journal_no": "JE-CREATE-001",
        "journal_date": "2025-01-10",
        "input_date": "2025-01-10",
        "settlement_flag": 0,
        "single_entry_flag": 1,
        "journal_type": 1,
        "recurring_flag": 0,
        "employee_code": null,
        "department_code": null,
        "reversal_flag": 0,
        "reversal_journal_no": null,
        "details": [
            {
                "journal_no": "JE-CREATE-001",
                "line_number": 1,
                "description": "テスト仕訳作成",
                "items": [
                    {
                        "journal_no": "JE-CREATE-001",
                        "line_number": 1,
                        "debit_credit_flag": "D",
                        "currency_code": "JPY",
                        "exchange_rate": "1.0",
                        "department_code": null,
                        "project_code": null,
                        "account_code": "1010",
                        "sub_account_code": null,
                        "amount": "50000.00",
                        "base_amount": "50000.00",
                        "tax_division": null,
                        "tax_rate": null,
                        "tax_calculation_type": null,
                        "due_date": null,
                        "cash_flow_flag": 0,
                        "segment_code": null,
                        "contra_account_code": null,
                        "contra_sub_account_code": null,
                        "note_code": null,
                        "note_content": null
                    },
                    {
                        "journal_no": "JE-CREATE-001",
                        "line_number": 1,
                        "debit_credit_flag": "C",
                        "currency_code": "JPY",
                        "exchange_rate": "1.0",
                        "department_code": null,
                        "project_code": null,
                        "account_code": "4100",
                        "sub_account_code": null,
                        "amount": "50000.00",
                        "base_amount": "50000.00",
                        "tax_division": null,
                        "tax_rate": null,
                        "tax_calculation_type": null,
                        "due_date": null,
                        "cash_flow_flag": 0,
                        "segment_code": null,
                        "contra_account_code": null,
                        "contra_sub_account_code": null,
                        "note_code": null,
                        "note_content": null
                    }
                ]
            }
        ]
    });

    let request = Request::builder()
        .uri("/api/v1/journals")
        .method("POST")
        .header("content-type", "application/json")
        .body(Body::from(serde_json::to_vec(&new_journal).unwrap()))
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
    let journal: serde_json::Value = serde_json::from_slice(&body).unwrap();

    assert_eq!(journal["journal_no"], "JE-CREATE-001");
    assert_eq!(journal["journal_date"], "2025-01-10");

    // 明細を確認
    let details = journal["details"].as_array().unwrap();
    assert_eq!(details.len(), 1);
    assert_eq!(details[0]["description"], "テスト仕訳作成");

    // 借方貸方項目を確認
    let items = details[0]["items"].as_array().unwrap();
    assert_eq!(items.len(), 2);
}

#[tokio::test]
async fn test_create_journal_with_empty_journal_no() {
    // Given: テストデータベース
    let db = TestDatabase::new().await;
    insert_test_accounts(&db.pool).await;

    let repository = Arc::new(JournalRepositoryImpl::new(db.pool.clone()));
    let service = JournalService::new(repository);
    let use_case: Arc<dyn JournalUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: 仕訳番号が空の状態でPOST
    let invalid_journal = json!({
        "journal_no": "",
        "journal_date": "2025-01-10",
        "input_date": "2025-01-10",
        "settlement_flag": 0,
        "single_entry_flag": 1,
        "journal_type": 1,
        "recurring_flag": 0,
        "employee_code": null,
        "department_code": null,
        "reversal_flag": 0,
        "reversal_journal_no": null,
        "details": []
    });

    let request = Request::builder()
        .uri("/api/v1/journals")
        .method("POST")
        .header("content-type", "application/json")
        .body(Body::from(serde_json::to_vec(&invalid_journal).unwrap()))
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Then: ステータスコードが400
    assert_eq!(response.status(), StatusCode::BAD_REQUEST);
}

#[tokio::test]
async fn test_create_journal_with_unbalanced_amounts() {
    // Given: テストデータベース
    let db = TestDatabase::new().await;
    insert_test_accounts(&db.pool).await;

    let repository = Arc::new(JournalRepositoryImpl::new(db.pool.clone()));
    let service = JournalService::new(repository);
    let use_case: Arc<dyn JournalUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: 借方と貸方の金額が不一致の仕訳を作成
    let unbalanced_journal = json!({
        "journal_no": "JE-UNBALANCED-001",
        "journal_date": "2025-01-10",
        "input_date": "2025-01-10",
        "settlement_flag": 0,
        "single_entry_flag": 1,
        "journal_type": 1,
        "recurring_flag": 0,
        "employee_code": null,
        "department_code": null,
        "reversal_flag": 0,
        "reversal_journal_no": null,
        "details": [
            {
                "journal_no": "JE-UNBALANCED-001",
                "line_number": 1,
                "description": "不均衡な仕訳",
                "items": [
                    {
                        "journal_no": "JE-UNBALANCED-001",
                        "line_number": 1,
                        "debit_credit_flag": "D",
                        "currency_code": "JPY",
                        "exchange_rate": "1.0",
                        "department_code": null,
                        "project_code": null,
                        "account_code": "1010",
                        "sub_account_code": null,
                        "amount": "100000.00",
                        "base_amount": "100000.00",
                        "tax_division": null,
                        "tax_rate": null,
                        "tax_calculation_type": null,
                        "due_date": null,
                        "cash_flow_flag": 0,
                        "segment_code": null,
                        "contra_account_code": null,
                        "contra_sub_account_code": null,
                        "note_code": null,
                        "note_content": null
                    },
                    {
                        "journal_no": "JE-UNBALANCED-001",
                        "line_number": 1,
                        "debit_credit_flag": "C",
                        "currency_code": "JPY",
                        "exchange_rate": "1.0",
                        "department_code": null,
                        "project_code": null,
                        "account_code": "4100",
                        "sub_account_code": null,
                        "amount": "50000.00",
                        "base_amount": "50000.00",
                        "tax_division": null,
                        "tax_rate": null,
                        "tax_calculation_type": null,
                        "due_date": null,
                        "cash_flow_flag": 0,
                        "segment_code": null,
                        "contra_account_code": null,
                        "contra_sub_account_code": null,
                        "note_code": null,
                        "note_content": null
                    }
                ]
            }
        ]
    });

    let request = Request::builder()
        .uri("/api/v1/journals")
        .method("POST")
        .header("content-type", "application/json")
        .body(Body::from(
            serde_json::to_vec(&unbalanced_journal).unwrap(),
        ))
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Then: ステータスコードが400（バランスエラー）
    assert_eq!(response.status(), StatusCode::BAD_REQUEST);
}
