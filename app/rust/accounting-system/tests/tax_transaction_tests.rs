mod common;

use common::{insert_account, TestDatabase};
use rust_decimal::Decimal;
use sqlx::Row;
use std::str::FromStr;

#[tokio::test]
async fn test_tax_transaction_initial_data() {
    let db = TestDatabase::new().await;

    // 初期データが投入されているか確認
    let rows = sqlx::query(r#"SELECT "課税取引コード", "課税取引名", "税率" FROM "課税取引マスタ" ORDER BY "課税取引コード""#)
        .fetch_all(&db.pool)
        .await
        .unwrap();

    assert_eq!(rows.len(), 4);

    // 課税取引
    assert_eq!(rows[0].get::<String, _>("課税取引コード"), "01");
    assert_eq!(rows[0].get::<String, _>("課税取引名"), "課税");
    assert_eq!(
        rows[0].get::<Decimal, _>("税率"),
        Decimal::from_str("0.100").unwrap()
    );

    // 非課税取引
    assert_eq!(rows[1].get::<String, _>("課税取引コード"), "02");
    assert_eq!(rows[1].get::<String, _>("課税取引名"), "非課税");

    // 免税取引
    assert_eq!(rows[2].get::<String, _>("課税取引コード"), "03");
    assert_eq!(rows[2].get::<String, _>("課税取引名"), "免税");

    // 不課税取引
    assert_eq!(rows[3].get::<String, _>("課税取引コード"), "04");
    assert_eq!(rows[3].get::<String, _>("課税取引名"), "不課税");
}

#[tokio::test]
async fn test_tax_rate_constraint() {
    let db = TestDatabase::new().await;

    // 無効な税率でエラーになるか確認（税率 > 1）
    let result = sqlx::query(
        r#"
        INSERT INTO "課税取引マスタ" ("課税取引コード", "課税取引名", "税率", "有効フラグ")
        VALUES ($1, $2, $3, $4)
        "#,
    )
    .bind("99")
    .bind("無効な税率")
    .bind(Decimal::from_str("1.5").unwrap()) // 150% (無効)
    .bind(true)
    .execute(&db.pool)
    .await;

    assert!(result.is_err());

    // 有効な税率で成功するか確認
    let result = sqlx::query(
        r#"
        INSERT INTO "課税取引マスタ" ("課税取引コード", "課税取引名", "税率", "有効フラグ")
        VALUES ($1, $2, $3, $4)
        "#,
    )
    .bind("05")
    .bind("軽減税率")
    .bind(Decimal::from_str("0.08").unwrap())
    .bind(true)
    .execute(&db.pool)
    .await;

    assert!(result.is_ok());
}

#[tokio::test]
async fn test_account_tax_code_foreign_key() {
    let db = TestDatabase::new().await;

    // 有効な課税取引コードで勘定科目を登録
    let result = sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "課税取引コード", "残高")
        VALUES ($1, $2, $3::account_type, $4, $5)
        RETURNING "勘定科目コード", "課税取引コード"
        "#,
    )
    .bind("5000")
    .bind("売上高")
    .bind("収益")
    .bind("01") // 課税取引コード（課税売上）
    .bind(Decimal::from_str("0").unwrap())
    .fetch_one(&db.pool)
    .await;

    assert!(result.is_ok());
    let row = result.unwrap();
    assert_eq!(row.get::<String, _>("勘定科目コード"), "5000");
    assert_eq!(
        row.get::<Option<String>, _>("課税取引コード"),
        Some("01".to_string())
    );

    // 存在しない課税取引コードで挿入を試みる
    let invalid_result = sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "課税取引コード", "残高")
        VALUES ($1, $2, $3::account_type, $4, $5)
        "#,
    )
    .bind("5100")
    .bind("その他売上")
    .bind("収益")
    .bind("99") // 存在しない課税取引コード
    .bind(Decimal::from_str("0").unwrap())
    .execute(&db.pool)
    .await;

    // 外部キー制約違反でエラーになることを期待
    assert!(invalid_result.is_err());
    let err_msg = invalid_result.unwrap_err().to_string();
    assert!(err_msg.contains("foreign key") || err_msg.contains("fk_account_tax_code"));
}

#[tokio::test]
async fn test_add_tax_code_to_account() {
    let db = TestDatabase::new().await;

    // 課税取引コードなしで勘定科目を登録
    insert_account(&db.pool, "1100", "普通預金", "資産", "0").await;

    // 課税取引コードを追加
    sqlx::query(
        r#"
        UPDATE "勘定科目マスタ"
        SET "課税取引コード" = $1, "更新日時" = CURRENT_TIMESTAMP
        WHERE "勘定科目コード" = $2
        "#,
    )
    .bind("01")
    .bind("1100")
    .execute(&db.pool)
    .await
    .expect("Failed to add tax code");

    // 更新されたか確認
    let row = sqlx::query(
        r#"
        SELECT "勘定科目コード", "課税取引コード"
        FROM "勘定科目マスタ"
        WHERE "勘定科目コード" = $1
        "#,
    )
    .bind("1100")
    .fetch_one(&db.pool)
    .await
    .unwrap();

    assert_eq!(
        row.get::<Option<String>, _>("課税取引コード"),
        Some("01".to_string())
    );
}

#[tokio::test]
async fn test_account_without_tax_code() {
    let db = TestDatabase::new().await;

    // 課税取引コードなしで勘定科目を登録（NULL許容）
    let result = sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
        VALUES ($1, $2, $3::account_type, $4)
        RETURNING "勘定科目コード", "課税取引コード"
        "#,
    )
    .bind("3000")
    .bind("資本金")
    .bind("純資産")
    .bind(Decimal::from_str("0").unwrap())
    .fetch_one(&db.pool)
    .await;

    assert!(result.is_ok());
    let row = result.unwrap();
    assert_eq!(row.get::<String, _>("勘定科目コード"), "3000");
    assert_eq!(row.get::<Option<String>, _>("課税取引コード"), None);
}

#[tokio::test]
async fn test_query_accounts_by_tax_code() {
    let db = TestDatabase::new().await;

    // 複数の勘定科目を登録
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "課税取引コード", "残高")
        VALUES
        ('4100', '売上', '収益', '01', 0),
        ('4200', '受取利息', '収益', '02', 0),
        ('3000', '資本金', '純資産', NULL, 0)
        "#,
    )
    .execute(&db.pool)
    .await
    .expect("Failed to insert test accounts");

    // 課税取引コード'01'の勘定科目を取得
    let rows = sqlx::query(
        r#"
        SELECT "勘定科目コード", "勘定科目名", "課税取引コード"
        FROM "勘定科目マスタ"
        WHERE "課税取引コード" = $1
        "#,
    )
    .bind("01")
    .fetch_all(&db.pool)
    .await
    .unwrap();

    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0].get::<String, _>("勘定科目コード"), "4100");
    assert_eq!(rows[0].get::<String, _>("勘定科目名"), "売上");
}
