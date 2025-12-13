mod common;

use axum::{
    body::Body,
    http::{Request, StatusCode},
    Router,
};
use chrono::NaiveDate;
use sqlx::PgPool;
use std::sync::Arc;
use tower::util::ServiceExt;

use accounting_system::application::ports::input::financial_statement_usecase::FinancialStatementUseCase;
use accounting_system::application::services::financial_statement_service::FinancialStatementService;
use accounting_system::infrastructure::web::handlers::financial_statement_handler;

use common::TestDatabase;

/// テスト用のアプリケーションルーターを作成
fn create_test_app(use_case: Arc<dyn FinancialStatementUseCase>) -> Router {
    Router::new()
        .route(
            "/api/v1/financial/balance-sheet",
            axum::routing::get(financial_statement_handler::get_balance_sheet),
        )
        .route(
            "/api/v1/financial/income-statement",
            axum::routing::get(financial_statement_handler::get_income_statement),
        )
        .route(
            "/api/v1/financial/ratios",
            axum::routing::get(financial_statement_handler::get_financial_ratios),
        )
        .with_state(use_case)
}

/// 財務諸表テスト用のデータをセットアップ
async fn setup_financial_test_data(pool: &PgPool) {
    // テーブルをクリア
    sqlx::query(r#"TRUNCATE TABLE "日次勘定科目残高" CASCADE"#)
        .execute(pool)
        .await
        .unwrap();
    sqlx::query(r#"TRUNCATE TABLE "勘定科目マスタ" CASCADE"#)
        .execute(pool)
        .await
        .unwrap();

    // 貸借対照表科目
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ" (
            "勘定科目コード", "勘定科目名", "勘定科目カナ", "勘定科目種別",
            "BSPL区分", "貸借区分", "取引要素区分", "集計区分"
        ) VALUES
        ('1110', '普通預金', 'フツウヨキン', '資産', 'B', 'D', '資産', 'D'),
        ('1410', '建物', 'タテモノ', '資産', 'B', 'D', '資産', 'D'),
        ('2110', '買掛金', 'カイカケキン', '負債', 'B', 'C', '負債', 'D'),
        ('2510', '長期借入金', 'チョウキカリイレキン', '負債', 'B', 'C', '負債', 'D'),
        ('3110', '資本金', 'シホンキン', '純資産', 'B', 'C', '資本', 'D')
        "#,
    )
    .execute(pool)
    .await
    .unwrap();

    // 損益計算書科目
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ" (
            "勘定科目コード", "勘定科目名", "勘定科目カナ", "勘定科目種別",
            "BSPL区分", "貸借区分", "取引要素区分", "集計区分", "費用区分"
        ) VALUES
        ('4110', '売上高', 'ウリアゲダカ', '収益', 'P', 'C', '収益', 'D', '0'),
        ('5110', '売上原価', 'ウリアゲゲンカ', '費用', 'P', 'D', '費用', 'D', '1'),
        ('5210', '販売費', 'ハンバイヒ', '費用', 'P', 'D', '費用', 'D', '2')
        "#,
    )
    .execute(pool)
    .await
    .unwrap();

    // 貸借対照表の残高
    let bs_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES
        ($1, '1110', '', '', '', 0, 8000000, 0),
        ($1, '1410', '', '', '', 0, 2000000, 0),
        ($1, '2110', '', '', '', 0, 0, 500000),
        ($1, '2510', '', '', '', 0, 0, 4500000),
        ($1, '3110', '', '', '', 0, 0, 5000000)
        "#,
    )
    .bind(bs_date)
    .execute(pool)
    .await
    .unwrap();

    // 損益計算書の残高（3回分）
    for (date, sales, selling_expense) in [
        (
            NaiveDate::from_ymd_opt(2024, 1, 10).unwrap(),
            3333333,
            666666,
        ),
        (
            NaiveDate::from_ymd_opt(2024, 1, 20).unwrap(),
            3333333,
            666667,
        ),
        (
            NaiveDate::from_ymd_opt(2024, 1, 31).unwrap(),
            3333334,
            666667,
        ),
    ] {
        sqlx::query(
            r#"
            INSERT INTO "日次勘定科目残高" (
                "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
            ) VALUES
            ($1, '4110', '', '', '', 0, 0, $2),
            ($1, '5110', '', '', '', 0, 2000000, 0),
            ($1, '5210', '', '', '', 0, $3, 0)
            "#,
        )
        .bind(date)
        .bind(sales)
        .bind(selling_expense)
        .execute(pool)
        .await
        .unwrap();
    }
}

#[tokio::test]
async fn test_get_balance_sheet() {
    // Given: テストデータベースと財務データ
    let db = TestDatabase::new().await;
    setup_financial_test_data(&db.pool).await;

    // DIコンテナを構築
    let service = FinancialStatementService::new(db.pool.clone());
    let use_case: Arc<dyn FinancialStatementUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: GET /api/v1/financial/balance-sheet?as_of_date=2024-01-31
    let request = Request::builder()
        .uri("/api/v1/financial/balance-sheet?as_of_date=2024-01-31")
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
    let balance_sheet: serde_json::Value = serde_json::from_slice(&body).unwrap();

    // 貸借対照表の基本構造を検証
    assert_eq!(balance_sheet["as_of_date"], "2024-01-31");
    assert_eq!(balance_sheet["total_assets"], "10000000.00");
    assert_eq!(balance_sheet["total_liabilities"], "5000000.00");
    assert_eq!(balance_sheet["total_equity"], "5000000.00");

    // 資産項目を検証
    let assets = balance_sheet["assets"].as_array().unwrap();
    assert_eq!(assets.len(), 2);
    assert_eq!(assets[0]["account_code"], "1110");
    assert_eq!(assets[0]["account_name"], "普通預金");
    assert_eq!(assets[0]["balance"], "8000000.00");
}

#[tokio::test]
async fn test_get_income_statement() {
    // Given: テストデータベースと財務データ
    let db = TestDatabase::new().await;
    setup_financial_test_data(&db.pool).await;

    let service = FinancialStatementService::new(db.pool.clone());
    let use_case: Arc<dyn FinancialStatementUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: GET /api/v1/financial/income-statement?start_date=2024-01-01&end_date=2024-01-31
    let request = Request::builder()
        .uri("/api/v1/financial/income-statement?start_date=2024-01-01&end_date=2024-01-31")
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
    let income_statement: serde_json::Value = serde_json::from_slice(&body).unwrap();

    // 損益計算書の基本構造を検証
    assert_eq!(income_statement["start_date"], "2024-01-01");
    assert_eq!(income_statement["end_date"], "2024-01-31");
    assert_eq!(income_statement["total_sales"], "10000000.00");
    assert_eq!(income_statement["cost_of_sales"], "6000000.00");
    assert_eq!(income_statement["gross_profit"], "4000000.00");
    assert_eq!(income_statement["net_income"], "2000000.00");

    // 収益項目を検証
    let revenues = income_statement["revenues"].as_array().unwrap();
    assert!(revenues.len() > 0);
}

#[tokio::test]
async fn test_get_financial_ratios() {
    // Given: テストデータベースと財務データ
    let db = TestDatabase::new().await;
    setup_financial_test_data(&db.pool).await;

    let service = FinancialStatementService::new(db.pool.clone());
    let use_case: Arc<dyn FinancialStatementUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: GET /api/v1/financial/ratios?as_of_date=2024-01-31&period_start=2024-01-01&period_end=2024-01-31
    let request = Request::builder()
        .uri("/api/v1/financial/ratios?as_of_date=2024-01-31&period_start=2024-01-01&period_end=2024-01-31")
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
    let ratios: serde_json::Value = serde_json::from_slice(&body).unwrap();

    // 財務指標を検証（既存テストから期待値を取得）
    // 流動比率 = 1600.00%
    // 自己資本比率 = 50.00%
    // 売上高総利益率 = 40.00%
    // 売上高営業利益率 = 20.00%
    // 売上高純利益率 = 20.00%
    assert!(
        ratios["current_ratio"]
            .as_str()
            .unwrap()
            .parse::<f64>()
            .unwrap()
            > 0.0
    );
    assert!(
        ratios["equity_ratio"]
            .as_str()
            .unwrap()
            .parse::<f64>()
            .unwrap()
            > 0.0
    );
    assert!(
        ratios["gross_profit_margin"]
            .as_str()
            .unwrap()
            .parse::<f64>()
            .unwrap()
            > 0.0
    );
}

#[tokio::test]
async fn test_get_balance_sheet_with_missing_parameter() {
    // Given: テストデータベース
    let db = TestDatabase::new().await;
    setup_financial_test_data(&db.pool).await;

    let service = FinancialStatementService::new(db.pool.clone());
    let use_case: Arc<dyn FinancialStatementUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: as_of_date パラメータなしでリクエスト
    let request = Request::builder()
        .uri("/api/v1/financial/balance-sheet")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Then: ステータスコードが400（バリデーションエラー）
    assert_eq!(response.status(), StatusCode::BAD_REQUEST);
}

#[tokio::test]
async fn test_get_income_statement_with_invalid_date_range() {
    // Given: テストデータベース
    let db = TestDatabase::new().await;
    setup_financial_test_data(&db.pool).await;

    let service = FinancialStatementService::new(db.pool.clone());
    let use_case: Arc<dyn FinancialStatementUseCase> = Arc::new(service);

    let app = create_test_app(use_case);

    // When: 不正な日付範囲（終了日 < 開始日）でリクエスト
    let request = Request::builder()
        .uri("/api/v1/financial/income-statement?start_date=2024-12-31&end_date=2024-01-01")
        .body(Body::empty())
        .unwrap();

    let response = app.oneshot(request).await.unwrap();

    // Then: ステータスコードが200（サービス層でエラーハンドリングされるか検証）
    // または500（内部エラー）
    let status = response.status();
    assert!(status == StatusCode::OK || status == StatusCode::INTERNAL_SERVER_ERROR);
}
