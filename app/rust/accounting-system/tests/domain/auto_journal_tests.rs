use crate::common::TestDatabase;
use chrono::{NaiveDateTime, Utc};

#[tokio::test]
async fn test_auto_journal_management_crud() {
    // Given: テスト用データベース
    let db = TestDatabase::new().await;

    // When: 自動仕訳管理レコードを登録
    let source_table = "売上データ";
    let last_processed_at = Utc::now().naive_utc();

    sqlx::query(
        r#"
        INSERT INTO "自動仕訳管理" (
            "source_table_name", "last_processed_at"
        ) VALUES ($1, $2)
        "#,
    )
    .bind(source_table)
    .bind(last_processed_at)
    .execute(&db.pool)
    .await
    .unwrap();

    // Then: データが正しく登録されている
    let count: i64 = sqlx::query_scalar(
        r#"SELECT COUNT(*) FROM "自動仕訳管理" WHERE "source_table_name" = $1"#,
    )
    .bind(source_table)
    .fetch_one(&db.pool)
    .await
    .unwrap();
    assert_eq!(count, 1);

    // When: 最終処理日時を更新
    let new_processed_at = Utc::now().naive_utc();
    sqlx::query(
        r#"
        UPDATE "自動仕訳管理"
        SET "last_processed_at" = $1
        WHERE "source_table_name" = $2
        "#,
    )
    .bind(new_processed_at)
    .bind(source_table)
    .execute(&db.pool)
    .await
    .unwrap();

    // Then: 更新が反映されている
    let updated_at: NaiveDateTime = sqlx::query_scalar(
        r#"SELECT "last_processed_at" FROM "自動仕訳管理" WHERE "source_table_name" = $1"#,
    )
    .bind(source_table)
    .fetch_one(&db.pool)
    .await
    .unwrap();
    assert!(updated_at >= last_processed_at);
}

#[tokio::test]
async fn test_auto_journal_pattern_with_items() {
    // Given: テスト用データベース
    let db = TestDatabase::new().await;

    // When: 自動仕訳パターンを登録
    let pattern_code = "SALES-001";
    let pattern_id: i64 = sqlx::query_scalar(
        r#"
        INSERT INTO "自動仕訳パターン" (
            "pattern_code", "pattern_name", "source_table_name", "is_active"
        ) VALUES ($1, $2, $3, $4)
        RETURNING "id"
        "#,
    )
    .bind(pattern_code)
    .bind("売上自動仕訳")
    .bind("売上データ")
    .bind(true)
    .fetch_one(&db.pool)
    .await
    .unwrap();

    // Then: パターンが登録されている
    assert!(pattern_id > 0);

    // When: パターン明細を登録（借方：売掛金、貸方：売上）
    sqlx::query(
        r#"
        INSERT INTO "自動仕訳パターン明細" (
            "pattern_id", "line_number", "debit_credit_flag", "account_code", "amount_expression"
        ) VALUES ($1, 1, 'D', '1130', 'total_amount')
        "#,
    )
    .bind(pattern_id)
    .execute(&db.pool)
    .await
    .unwrap();

    sqlx::query(
        r#"
        INSERT INTO "自動仕訳パターン明細" (
            "pattern_id", "line_number", "debit_credit_flag", "account_code", "amount_expression"
        ) VALUES ($1, 2, 'C', '4100', 'total_amount')
        "#,
    )
    .bind(pattern_id)
    .execute(&db.pool)
    .await
    .unwrap();

    // Then: パターン明細が2件登録されている
    let item_count: i64 = sqlx::query_scalar(
        r#"SELECT COUNT(*) FROM "自動仕訳パターン明細" WHERE "pattern_id" = $1"#,
    )
    .bind(pattern_id)
    .fetch_one(&db.pool)
    .await
    .unwrap();
    assert_eq!(item_count, 2);
}

#[tokio::test]
async fn test_auto_journal_execution_log() {
    // Given: テスト用データベースとパターン
    let db = TestDatabase::new().await;

    let pattern_id: i64 = sqlx::query_scalar(
        r#"
        INSERT INTO "自動仕訳パターン" (
            "pattern_code", "pattern_name", "source_table_name", "is_active"
        ) VALUES ($1, $2, $3, $4)
        RETURNING "id"
        "#,
    )
    .bind("PAYROLL-001")
    .bind("給与自動仕訳")
    .bind("給与データ")
    .bind(true)
    .fetch_one(&db.pool)
    .await
    .unwrap();

    // When: 実行ログを登録（成功）
    let executed_at = Utc::now().naive_utc();
    sqlx::query(
        r#"
        INSERT INTO "自動仕訳実行ログ" (
            "pattern_id", "executed_at", "processed_count", "generated_count", "status"
        ) VALUES ($1, $2, $3, $4, $5)
        "#,
    )
    .bind(pattern_id)
    .bind(executed_at)
    .bind(100)
    .bind(100)
    .bind("success")
    .execute(&db.pool)
    .await
    .unwrap();

    // Then: ログが登録されている
    let log_count: i64 = sqlx::query_scalar(
        r#"SELECT COUNT(*) FROM "自動仕訳実行ログ" WHERE "pattern_id" = $1"#,
    )
    .bind(pattern_id)
    .fetch_one(&db.pool)
    .await
    .unwrap();
    assert_eq!(log_count, 1);

    // When: 失敗ログを登録
    sqlx::query(
        r#"
        INSERT INTO "自動仕訳実行ログ" (
            "pattern_id", "executed_at", "processed_count", "generated_count", "status", "error_detail"
        ) VALUES ($1, $2, $3, $4, $5, $6)
        "#,
    )
    .bind(pattern_id)
    .bind(Utc::now().naive_utc())
    .bind(50)
    .bind(0)
    .bind("error")
    .bind("データベース接続エラー")
    .execute(&db.pool)
    .await
    .unwrap();

    // Then: ログが2件になっている
    let log_count: i64 = sqlx::query_scalar(
        r#"SELECT COUNT(*) FROM "自動仕訳実行ログ" WHERE "pattern_id" = $1"#,
    )
    .bind(pattern_id)
    .fetch_one(&db.pool)
    .await
    .unwrap();
    assert_eq!(log_count, 2);

    // Then: エラーログが取得できる
    let error_count: i64 = sqlx::query_scalar(
        r#"SELECT COUNT(*) FROM "自動仕訳実行ログ" WHERE "pattern_id" = $1 AND "status" = 'error'"#,
    )
    .bind(pattern_id)
    .fetch_one(&db.pool)
    .await
    .unwrap();
    assert_eq!(error_count, 1);
}

#[tokio::test]
async fn test_auto_journal_pattern_cascade_delete() {
    // Given: テスト用データベースとパターン
    let db = TestDatabase::new().await;

    let pattern_id: i64 = sqlx::query_scalar(
        r#"
        INSERT INTO "自動仕訳パターン" (
            "pattern_code", "pattern_name", "source_table_name", "is_active"
        ) VALUES ($1, $2, $3, $4)
        RETURNING "id"
        "#,
    )
    .bind("TEST-001")
    .bind("テストパターン")
    .bind("テストデータ")
    .bind(true)
    .fetch_one(&db.pool)
    .await
    .unwrap();

    // パターン明細を登録
    sqlx::query(
        r#"
        INSERT INTO "自動仕訳パターン明細" (
            "pattern_id", "line_number", "debit_credit_flag", "account_code", "amount_expression"
        ) VALUES ($1, 1, 'D', '1010', 'amount')
        "#,
    )
    .bind(pattern_id)
    .execute(&db.pool)
    .await
    .unwrap();

    // 実行ログを登録
    sqlx::query(
        r#"
        INSERT INTO "自動仕訳実行ログ" (
            "pattern_id", "executed_at", "processed_count", "generated_count", "status"
        ) VALUES ($1, $2, $3, $4, $5)
        "#,
    )
    .bind(pattern_id)
    .bind(Utc::now().naive_utc())
    .bind(10)
    .bind(10)
    .bind("success")
    .execute(&db.pool)
    .await
    .unwrap();

    // When: パターンを削除
    sqlx::query(r#"DELETE FROM "自動仕訳パターン" WHERE "id" = $1"#)
        .bind(pattern_id)
        .execute(&db.pool)
        .await
        .unwrap();

    // Then: パターン明細も自動削除される（CASCADE）
    let item_count: i64 = sqlx::query_scalar(
        r#"SELECT COUNT(*) FROM "自動仕訳パターン明細" WHERE "pattern_id" = $1"#,
    )
    .bind(pattern_id)
    .fetch_one(&db.pool)
    .await
    .unwrap();
    assert_eq!(item_count, 0);

    // Then: 実行ログも自動削除される（CASCADE）
    let log_count: i64 = sqlx::query_scalar(
        r#"SELECT COUNT(*) FROM "自動仕訳実行ログ" WHERE "pattern_id" = $1"#,
    )
    .bind(pattern_id)
    .fetch_one(&db.pool)
    .await
    .unwrap();
    assert_eq!(log_count, 0);
}

#[tokio::test]
async fn test_auto_journal_unique_constraints() {
    // Given: テスト用データベース
    let db = TestDatabase::new().await;

    // When: 同じソーステーブル名で2回登録しようとする
    let source_table = "売上データ";
    sqlx::query(
        r#"
        INSERT INTO "自動仕訳管理" (
            "source_table_name", "last_processed_at"
        ) VALUES ($1, $2)
        "#,
    )
    .bind(source_table)
    .bind(Utc::now().naive_utc())
    .execute(&db.pool)
    .await
    .unwrap();

    // Then: 2回目の登録は失敗する（UNIQUE制約）
    let result = sqlx::query(
        r#"
        INSERT INTO "自動仕訳管理" (
            "source_table_name", "last_processed_at"
        ) VALUES ($1, $2)
        "#,
    )
    .bind(source_table)
    .bind(Utc::now().naive_utc())
    .execute(&db.pool)
    .await;

    assert!(result.is_err());

    // When: 同じパターンコードで2回登録しようとする
    let pattern_code = "SALES-001";
    sqlx::query(
        r#"
        INSERT INTO "自動仕訳パターン" (
            "pattern_code", "pattern_name", "source_table_name", "is_active"
        ) VALUES ($1, $2, $3, $4)
        "#,
    )
    .bind(pattern_code)
    .bind("売上パターン1")
    .bind("売上データ")
    .bind(true)
    .execute(&db.pool)
    .await
    .unwrap();

    // Then: 2回目の登録は失敗する（UNIQUE制約）
    let result = sqlx::query(
        r#"
        INSERT INTO "自動仕訳パターン" (
            "pattern_code", "pattern_name", "source_table_name", "is_active"
        ) VALUES ($1, $2, $3, $4)
        "#,
    )
    .bind(pattern_code)
    .bind("売上パターン2")
    .bind("売上データ")
    .bind(true)
    .execute(&db.pool)
    .await;

    assert!(result.is_err());
}
