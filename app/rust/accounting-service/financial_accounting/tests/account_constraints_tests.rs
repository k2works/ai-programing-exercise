mod common;

use common::TestDatabase;
use rust_decimal::Decimal;
use sqlx::Row;
use std::str::FromStr;

#[tokio::test]
async fn test_bspl_type_field() {
    let db = TestDatabase::new().await;

    // BSPL区分を含む勘定科目を登録
    let row = sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "貸借区分", "残高")
        VALUES ($1, $2, $3::account_type, $4, $5, $6)
        RETURNING "勘定科目コード", "貸借区分"
        "#,
    )
    .bind("1000")
    .bind("現金")
    .bind("資産")
    .bind("B")
    .bind("D") // 借方
    .bind(Decimal::from_str("0").unwrap())
    .fetch_one(&db.pool)
    .await;

    assert!(row.is_ok());
    let row = row.unwrap();
    assert_eq!(row.get::<String, _>("勘定科目コード"), "1000");
}

#[tokio::test]
async fn test_debit_credit_type_field() {
    let db = TestDatabase::new().await;

    // 借方区分（D）の勘定科目を登録
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "貸借区分", "残高")
        VALUES ($1, $2, $3::account_type, $4, $5)
        "#,
    )
    .bind("1000")
    .bind("現金")
    .bind("資産")
    .bind("D")
    .bind(Decimal::from_str("0").unwrap())
    .execute(&db.pool)
    .await
    .expect("Failed to insert debit account");

    // 貸方区分（C）の勘定科目を登録
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "貸借区分", "残高")
        VALUES ($1, $2, $3::account_type, $4, $5)
        "#,
    )
    .bind("2000")
    .bind("買掛金")
    .bind("負債")
    .bind("C")
    .bind(Decimal::from_str("0").unwrap())
    .execute(&db.pool)
    .await
    .expect("Failed to insert credit account");

    // 登録されたか確認
    let count = sqlx::query_scalar::<_, i64>(
        r#"SELECT COUNT(*) FROM "勘定科目マスタ" WHERE "貸借区分" IN ('D', 'C')"#,
    )
    .fetch_one(&db.pool)
    .await
    .unwrap();

    assert_eq!(count, 2);
}

#[tokio::test]
async fn test_aggregation_type_field() {
    let db = TestDatabase::new().await;

    // 集計区分を含む勘定科目を登録
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "集計区分", "残高")
        VALUES ($1, $2, $3::account_type, $4, $5)
        "#,
    )
    .bind("11")
    .bind("資産の部")
    .bind("資産")
    .bind("H") // 見出科目
    .bind(Decimal::from_str("0").unwrap())
    .execute(&db.pool)
    .await
    .expect("Failed to insert aggregation account");

    // 登録されたか確認
    let row = sqlx::query(
        r#"
        SELECT "勘定科目コード", "集計区分"
        FROM "勘定科目マスタ"
        WHERE "勘定科目コード" = $1
        "#,
    )
    .bind("11")
    .fetch_one(&db.pool)
    .await
    .unwrap();

    assert_eq!(row.get::<String, _>("勘定科目コード"), "11");
    assert_eq!(
        row.get::<Option<String>, _>("集計区分"),
        Some("H".to_string())
    );
}

#[tokio::test]
async fn test_bspl_constraint_invalid_value() {
    let db = TestDatabase::new().await;

    // 無効なBSPL区分値でエラーになるか確認
    let result = sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高")
        VALUES ($1, $2, $3::account_type, $4, $5)
        "#,
    )
    .bind("1000")
    .bind("現金")
    .bind("資産")
    .bind("X") // 無効な値
    .bind(Decimal::from_str("0").unwrap())
    .execute(&db.pool)
    .await;

    assert!(result.is_err());
}

#[tokio::test]
async fn test_bspl_consistency_constraint() {
    let db = TestDatabase::new().await;

    // BSPL区分Bと資産種別の組み合わせ（正常）
    let result = sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高")
        VALUES ($1, $2, $3::account_type, $4, $5)
        "#,
    )
    .bind("1000")
    .bind("現金")
    .bind("資産")
    .bind("B")
    .bind(Decimal::from_str("0").unwrap())
    .execute(&db.pool)
    .await;

    assert!(result.is_ok());

    // BSPL区分Bと費用種別の組み合わせ（エラー）
    let result = sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ"
        ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高")
        VALUES ($1, $2, $3::account_type, $4, $5)
        "#,
    )
    .bind("6000")
    .bind("旅費交通費")
    .bind("費用")
    .bind("B") // 費用はPでなければならない
    .bind(Decimal::from_str("0").unwrap())
    .execute(&db.pool)
    .await;

    assert!(result.is_err());
}
