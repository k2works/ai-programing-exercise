mod common;

use common::{insert_account, TestDatabase};
use rust_decimal::Decimal;
use sqlx::Row;
use std::str::FromStr;

#[tokio::test]
async fn test_database_connection() {
    let db = TestDatabase::new().await;

    // 接続確認用の簡単なクエリ
    let result: (i32,) = sqlx::query_as("SELECT 1")
        .fetch_one(&db.pool)
        .await
        .expect("Failed to execute query");

    assert_eq!(result.0, 1);
}

#[tokio::test]
async fn test_create_account() {
    let db = TestDatabase::new().await;

    // 勘定科目を登録
    let row = sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
        VALUES ($1, $2, $3::account_type, $4)
        RETURNING "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目種別"::TEXT, "残高"
        "#,
    )
    .bind("1000")
    .bind("現金")
    .bind("資産")
    .bind(Decimal::from_str("50000.00").unwrap())
    .fetch_one(&db.pool)
    .await
    .expect("Failed to insert account");

    // 結果を検証
    assert_eq!(row.get::<String, _>("勘定科目コード"), "1000");
    assert_eq!(row.get::<String, _>("勘定科目名"), "現金");
    assert_eq!(row.get::<String, _>("勘定科目種別"), "資産");
    assert_eq!(
        row.get::<Decimal, _>("残高"),
        Decimal::from_str("50000.00").unwrap()
    );
}

#[tokio::test]
async fn test_find_all_accounts() {
    let db = TestDatabase::new().await;

    // 1. 複数の勘定科目を登録
    insert_account(&db.pool, "1000", "現金", "資産", "50000.00").await;
    insert_account(&db.pool, "2000", "買掛金", "負債", "30000.00").await;
    insert_account(&db.pool, "3000", "資本金", "純資産", "100000.00").await;

    // 2. すべての勘定科目を取得
    let rows = sqlx::query(
        r#"
        SELECT "勘定科目コード", "勘定科目名", "勘定科目種別", "残高"
        FROM "勘定科目マスタ"
        ORDER BY "勘定科目コード"
        "#,
    )
    .fetch_all(&db.pool)
    .await
    .expect("Failed to fetch accounts");

    // 3. 期待通りのデータが取得できるか検証
    assert_eq!(rows.len(), 3);
    assert_eq!(rows[0].get::<String, _>("勘定科目コード"), "1000");
    assert_eq!(rows[1].get::<String, _>("勘定科目コード"), "2000");
    assert_eq!(rows[2].get::<String, _>("勘定科目コード"), "3000");
}

#[tokio::test]
async fn test_find_account_by_code() {
    let db = TestDatabase::new().await;

    // 1. テストデータを登録
    insert_account(&db.pool, "1000", "現金", "資産", "50000.00").await;

    // 2. コードで検索
    let row = sqlx::query(
        r#"
        SELECT "勘定科目コード", "勘定科目名", "勘定科目種別"::TEXT
        FROM "勘定科目マスタ"
        WHERE "勘定科目コード" = $1
        "#,
    )
    .bind("1000")
    .fetch_one(&db.pool)
    .await
    .expect("Failed to fetch account");

    // 3. 結果を検証
    assert_eq!(row.get::<String, _>("勘定科目コード"), "1000");
    assert_eq!(row.get::<String, _>("勘定科目名"), "現金");
    assert_eq!(row.get::<String, _>("勘定科目種別"), "資産");
}

#[tokio::test]
async fn test_update_account() {
    let db = TestDatabase::new().await;

    // 1. テストデータを登録
    let account_id = insert_account(&db.pool, "1000", "現金", "資産", "50000.00").await;

    // 2. 勘定科目を更新
    sqlx::query(
        r#"
        UPDATE "勘定科目マスタ"
        SET "勘定科目名" = $1, "残高" = $2, "更新日時" = CURRENT_TIMESTAMP
        WHERE "勘定科目ID" = $3
        "#,
    )
    .bind("現金及び預金")
    .bind(Decimal::from_str("75000.00").unwrap())
    .bind(account_id)
    .execute(&db.pool)
    .await
    .expect("Failed to update account");

    // 3. 更新されたか検証
    let row = sqlx::query(
        r#"
        SELECT "勘定科目名", "残高"
        FROM "勘定科目マスタ"
        WHERE "勘定科目ID" = $1
        "#,
    )
    .bind(account_id)
    .fetch_one(&db.pool)
    .await
    .expect("Failed to fetch updated account");

    assert_eq!(row.get::<String, _>("勘定科目名"), "現金及び預金");
    assert_eq!(
        row.get::<Decimal, _>("残高"),
        Decimal::from_str("75000.00").unwrap()
    );
}
