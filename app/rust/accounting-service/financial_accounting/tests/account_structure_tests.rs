mod common;

use common::{insert_account, TestDatabase};
use sqlx::Row;

#[tokio::test]
async fn test_create_account_structure() {
    let db = TestDatabase::new().await;

    // まず勘定科目を登録
    insert_account(&db.pool, "11110", "現金", "資産", "0").await;

    // 勘定科目構成を登録
    sqlx::query(
        r#"
        INSERT INTO "勘定科目構成マスタ"
        ("勘定科目コード", "勘定科目パス", "階層レベル", "親科目コード", "表示順序")
        VALUES ($1, $2, $3, $4, $5)
        "#,
    )
    .bind("11110")
    .bind("11~11000~11190~11110")
    .bind(4)
    .bind("11190")
    .bind(10)
    .execute(&db.pool)
    .await
    .expect("Failed to insert account structure");

    // 登録されたか確認
    let row = sqlx::query(
        r#"
        SELECT "勘定科目コード", "勘定科目パス", "階層レベル"
        FROM "勘定科目構成マスタ"
        WHERE "勘定科目コード" = $1
        "#,
    )
    .bind("11110")
    .fetch_one(&db.pool)
    .await
    .unwrap();

    assert_eq!(row.get::<String, _>("勘定科目コード"), "11110");
    assert_eq!(row.get::<String, _>("勘定科目パス"), "11~11000~11190~11110");
    assert_eq!(row.get::<i32, _>("階層レベル"), 4);
}

#[tokio::test]
async fn test_create_hierarchical_accounts() {
    let db = TestDatabase::new().await;

    // 階層構造の勘定科目を登録
    insert_account(&db.pool, "11", "資産の部", "資産", "0").await;
    insert_account(&db.pool, "11000", "流動資産", "資産", "0").await;
    insert_account(&db.pool, "11190", "現預金", "資産", "0").await;
    insert_account(&db.pool, "11110", "現金", "資産", "0").await;

    // 各レベルの構成を登録
    sqlx::query(
        r#"
        INSERT INTO "勘定科目構成マスタ"
        ("勘定科目コード", "勘定科目パス", "階層レベル", "表示順序")
        VALUES
        ('11', '11', 1, 1),
        ('11000', '11~11000', 2, 1),
        ('11190', '11~11000~11190', 3, 1),
        ('11110', '11~11000~11190~11110', 4, 1)
        "#,
    )
    .execute(&db.pool)
    .await
    .expect("Failed to insert hierarchical structures");

    // 階層レベル2の勘定科目を取得
    let rows = sqlx::query(
        r#"
        SELECT "勘定科目コード", "階層レベル"
        FROM "勘定科目構成マスタ"
        WHERE "階層レベル" = 2
        "#,
    )
    .fetch_all(&db.pool)
    .await
    .unwrap();

    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0].get::<String, _>("勘定科目コード"), "11000");
}
