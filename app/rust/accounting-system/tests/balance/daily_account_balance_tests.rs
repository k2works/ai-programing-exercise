use crate::common::TestDatabase;
use chrono::NaiveDate;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;

/// テスト用勘定科目を登録するヘルパー関数
async fn insert_test_accounts(db: &TestDatabase) {
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
        VALUES
        ('1020', '普通預金', '資産', false, true, 0),
        ('1130', '売掛金', '資産', false, true, 0),
        ('4010', '売上高', '収益', false, true, 0),
        ('5010', '売上原価', '費用', false, true, 0)
        ON CONFLICT ("勘定科目コード") DO NOTHING
        "#,
    )
    .execute(&db.pool)
    .await
    .unwrap();
}

#[tokio::test]
async fn test_insert_daily_balance() {
    // Given: 2025-01-15 の普通預金の日次残高
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let entry_date = NaiveDate::from_ymd_opt(2025, 1, 15).unwrap();
    let account_code = "1020"; // 普通預金

    // When: 日次残高を登録
    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES ($1, $2, '', '', '', 0, 100000.00, 0.00)
        "#,
    )
    .bind(entry_date)
    .bind(account_code)
    .execute(&db.pool)
    .await
    .unwrap();

    // Then: データが正しく登録されている
    let (debit_amount, credit_amount): (Decimal, Decimal) = sqlx::query_as(
        r#"
        SELECT "借方金額", "貸方金額"
        FROM "日次勘定科目残高"
        WHERE "起票日" = $1
          AND "勘定科目コード" = $2
        "#,
    )
    .bind(entry_date)
    .bind(account_code)
    .fetch_one(&db.pool)
    .await
    .unwrap();

    assert_eq!(debit_amount, dec!(100000.00));
    assert_eq!(credit_amount, dec!(0.00));
}

#[tokio::test]
async fn test_composite_primary_key_uniqueness() {
    // Given: 同じキーで日次残高を登録
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let entry_date = NaiveDate::from_ymd_opt(2025, 1, 15).unwrap();
    let account_code = "1020";

    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES ($1, $2, '', '', '', 0, 100000.00, 0.00)
        "#,
    )
    .bind(entry_date)
    .bind(account_code)
    .execute(&db.pool)
    .await
    .unwrap();

    // When & Then: 同じキーで2回目の登録を試みるとエラー
    let result = sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES ($1, $2, '', '', '', 0, 50000.00, 0.00)
        "#,
    )
    .bind(entry_date)
    .bind(account_code)
    .execute(&db.pool)
    .await;

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("duplicate key"));
}

#[tokio::test]
async fn test_department_balance() {
    // Given: 売上高の部門別日次残高
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let entry_date = NaiveDate::from_ymd_opt(2025, 1, 15).unwrap();
    let account_code = "4010"; // 売上高

    // When: 部門001と部門002の残高を登録
    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES ($1, $2, '', '001', '', 0, 0.00, 300000.00)
        "#,
    )
    .bind(entry_date)
    .bind(account_code)
    .execute(&db.pool)
    .await
    .unwrap();

    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES ($1, $2, '', '002', '', 0, 0.00, 200000.00)
        "#,
    )
    .bind(entry_date)
    .bind(account_code)
    .execute(&db.pool)
    .await
    .unwrap();

    // Then: 部門別に集計できる
    let results: Vec<(String, Decimal)> = sqlx::query_as(
        r#"
        SELECT "部門コード", SUM("貸方金額") as 売上合計
        FROM "日次勘定科目残高"
        WHERE "勘定科目コード" = $1
        GROUP BY "部門コード"
        ORDER BY "部門コード"
        "#,
    )
    .bind(account_code)
    .fetch_all(&db.pool)
    .await
    .unwrap();

    assert_eq!(results.len(), 2);
    assert_eq!(results[0].0, "001");
    assert_eq!(results[0].1, dec!(300000.00));
    assert_eq!(results[1].0, "002");
    assert_eq!(results[1].1, dec!(200000.00));
}

#[tokio::test]
async fn test_project_balance() {
    // Given: プロジェクト別の残高
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let entry_date = NaiveDate::from_ymd_opt(2025, 1, 15).unwrap();
    let account_code = "4010"; // 売上高

    // When: プロジェクトP001とP002の残高を登録
    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES ($1, $2, '', '', 'P001', 0, 0.00, 150000.00)
        "#,
    )
    .bind(entry_date)
    .bind(account_code)
    .execute(&db.pool)
    .await
    .unwrap();

    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES ($1, $2, '', '', 'P002', 0, 0.00, 250000.00)
        "#,
    )
    .bind(entry_date)
    .bind(account_code)
    .execute(&db.pool)
    .await
    .unwrap();

    // Then: プロジェクト別に集計できる
    let results: Vec<(String, Decimal)> = sqlx::query_as(
        r#"
        SELECT "プロジェクトコード", SUM("貸方金額") as 売上合計
        FROM "日次勘定科目残高"
        WHERE "勘定科目コード" = $1
        GROUP BY "プロジェクトコード"
        ORDER BY "プロジェクトコード"
        "#,
    )
    .bind(account_code)
    .fetch_all(&db.pool)
    .await
    .unwrap();

    assert_eq!(results.len(), 2);
    assert_eq!(results[0].0, "P001");
    assert_eq!(results[0].1, dec!(150000.00));
    assert_eq!(results[1].0, "P002");
    assert_eq!(results[1].1, dec!(250000.00));
}

#[tokio::test]
async fn test_sub_account_balance() {
    // Given: 売掛金の補助科目（得意先）別残高
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let entry_date = NaiveDate::from_ymd_opt(2025, 1, 15).unwrap();
    let account_code = "1130"; // 売掛金

    // When: 得意先A001とA002の残高を登録
    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES ($1, $2, 'A001', '', '', 0, 500000.00, 0.00)
        "#,
    )
    .bind(entry_date)
    .bind(account_code)
    .execute(&db.pool)
    .await
    .unwrap();

    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES ($1, $2, 'A002', '', '', 0, 300000.00, 0.00)
        "#,
    )
    .bind(entry_date)
    .bind(account_code)
    .execute(&db.pool)
    .await
    .unwrap();

    // Then: 補助科目別に集計できる
    let results: Vec<(String, Decimal)> = sqlx::query_as(
        r#"
        SELECT "補助科目コード", SUM("借方金額") as 売掛金合計
        FROM "日次勘定科目残高"
        WHERE "勘定科目コード" = $1
        GROUP BY "補助科目コード"
        ORDER BY "補助科目コード"
        "#,
    )
    .bind(account_code)
    .fetch_all(&db.pool)
    .await
    .unwrap();

    assert_eq!(results.len(), 2);
    assert_eq!(results[0].0, "A001");
    assert_eq!(results[0].1, dec!(500000.00));
    assert_eq!(results[1].0, "A002");
    assert_eq!(results[1].1, dec!(300000.00));
}

#[tokio::test]
async fn test_settlement_flag_separation() {
    // Given: 通常仕訳と決算仕訳の残高
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let entry_date = NaiveDate::from_ymd_opt(2025, 3, 31).unwrap();
    let account_code = "5010"; // 売上原価

    // When: 通常仕訳（決算仕訳フラグ=0）の残高を登録
    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES ($1, $2, '', '', '', 0, 800000.00, 0.00)
        "#,
    )
    .bind(entry_date)
    .bind(account_code)
    .execute(&db.pool)
    .await
    .unwrap();

    // 決算仕訳（決算仕訳フラグ=1）の残高を登録
    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES ($1, $2, '', '', '', 1, 0.00, 800000.00)
        "#,
    )
    .bind(entry_date)
    .bind(account_code)
    .execute(&db.pool)
    .await
    .unwrap();

    // Then: 決算仕訳フラグで分けて集計できる
    let results: Vec<(i32, Decimal, Decimal)> = sqlx::query_as(
        r#"
        SELECT "決算仕訳フラグ", SUM("借方金額") as 借方合計, SUM("貸方金額") as 貸方合計
        FROM "日次勘定科目残高"
        WHERE "勘定科目コード" = $1
        GROUP BY "決算仕訳フラグ"
        ORDER BY "決算仕訳フラグ"
        "#,
    )
    .bind(account_code)
    .fetch_all(&db.pool)
    .await
    .unwrap();

    assert_eq!(results.len(), 2);

    // 通常仕訳
    assert_eq!(results[0].0, 0);
    assert_eq!(results[0].1, dec!(800000.00));
    assert_eq!(results[0].2, dec!(0.00));

    // 決算仕訳
    assert_eq!(results[1].0, 1);
    assert_eq!(results[1].1, dec!(0.00));
    assert_eq!(results[1].2, dec!(800000.00));
}
