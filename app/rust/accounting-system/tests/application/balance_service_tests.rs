use crate::common::TestDatabase;
use accounting_system::application::balance::balance_service::BalanceService;
use chrono::NaiveDate;
use rust_decimal::Decimal;
use rust_decimal_macros::dec;

/// テスト用勘定科目を登録するヘルパー関数
async fn insert_test_accounts(db: &TestDatabase) {
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
        VALUES
        ('1010', '現金', '資産', false, true, 0),
        ('1020', '普通預金', '資産', false, true, 0),
        ('4010', '売上高', '収益', false, true, 0)
        ON CONFLICT ("勘定科目コード") DO NOTHING
        "#,
    )
    .execute(&db.pool)
    .await
    .unwrap();
}

#[tokio::test]
async fn test_update_daily_balance_new_record() {
    // Given: 新しい日次残高
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let service = BalanceService::new(db.pool.clone());
    let entry_date = NaiveDate::from_ymd_opt(2025, 1, 15).unwrap();

    // When: 日次残高を更新
    service
        .update_daily_balance(
            entry_date,
            "1010",
            None,
            None,
            None,
            None,
            dec!(100000.00),
            dec!(0.00),
        )
        .await
        .unwrap();

    // Then: データが正しく登録されている
    let (debit_amount, credit_amount): (Decimal, Decimal) = sqlx::query_as(
        r#"
        SELECT "借方金額", "貸方金額"
        FROM "日次勘定科目残高"
        WHERE "起票日" = $1 AND "勘定科目コード" = $2
        "#,
    )
    .bind(entry_date)
    .bind("1010")
    .fetch_one(&db.pool)
    .await
    .unwrap();

    assert_eq!(debit_amount, dec!(100000.00));
    assert_eq!(credit_amount, dec!(0.00));
}

#[tokio::test]
async fn test_update_daily_balance_accumulation() {
    // Given: 既存の日次残高がある
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let service = BalanceService::new(db.pool.clone());
    let entry_date = NaiveDate::from_ymd_opt(2025, 1, 15).unwrap();

    // 初回登録
    service
        .update_daily_balance(
            entry_date,
            "1010",
            None,
            None,
            None,
            None,
            dec!(100000.00),
            dec!(0.00),
        )
        .await
        .unwrap();

    // When: 同じキーで再度更新
    service
        .update_daily_balance(
            entry_date,
            "1010",
            None,
            None,
            None,
            None,
            dec!(50000.00),
            dec!(30000.00),
        )
        .await
        .unwrap();

    // Then: 金額が累積されている
    let (debit_amount, credit_amount): (Decimal, Decimal) = sqlx::query_as(
        r#"
        SELECT "借方金額", "貸方金額"
        FROM "日次勘定科目残高"
        WHERE "起票日" = $1 AND "勘定科目コード" = $2
        "#,
    )
    .bind(entry_date)
    .bind("1010")
    .fetch_one(&db.pool)
    .await
    .unwrap();

    assert_eq!(debit_amount, dec!(150000.00)); // 100000 + 50000
    assert_eq!(credit_amount, dec!(30000.00)); // 0 + 30000
}

#[tokio::test]
async fn test_update_daily_balance_with_sub_account() {
    // Given: 補助科目付きの残高
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let service = BalanceService::new(db.pool.clone());
    let entry_date = NaiveDate::from_ymd_opt(2025, 1, 15).unwrap();

    // When: 補助科目を指定して更新
    service
        .update_daily_balance(
            entry_date,
            "1010",
            Some("SUB001"),
            None,
            None,
            None,
            dec!(100000.00),
            dec!(0.00),
        )
        .await
        .unwrap();

    // Then: 補助科目が正しく記録されている
    let (sub_account_code, debit_amount): (String, Decimal) = sqlx::query_as(
        r#"
        SELECT "補助科目コード", "借方金額"
        FROM "日次勘定科目残高"
        WHERE "起票日" = $1 AND "勘定科目コード" = $2
        "#,
    )
    .bind(entry_date)
    .bind("1010")
    .fetch_one(&db.pool)
    .await
    .unwrap();

    assert_eq!(sub_account_code, "SUB001");
    assert_eq!(debit_amount, dec!(100000.00));
}

#[tokio::test]
async fn test_update_balance_from_journal_items() {
    // Given: 仕訳が登録されている
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let journal_no = "JE-BAL-001";
    let journal_date = NaiveDate::from_ymd_opt(2025, 1, 15).unwrap();

    // 仕訳を登録
    let mut tx = db.pool.begin().await.unwrap();

    // 仕訳ヘッダー
    sqlx::query(
        r#"
        INSERT INTO "仕訳" (
            "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
            "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES ($1, $2, $2, 0, 1, 0, 0, 0)
        "#,
    )
    .bind(journal_no)
    .bind(journal_date)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 仕訳明細
    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "仕訳伝票番号", "仕訳行番号", "行摘要"
        ) VALUES ($1, 1, 'テスト仕訳')
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 借方明細
    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
            "通貨コード", "為替レート", "勘定科目コード",
            "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 1, 'D', 'JPY', 1.0, '1010', 100000.00, 100000.00, 0)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 貸方明細
    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
            "通貨コード", "為替レート", "勘定科目コード",
            "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 1, 'C', 'JPY', 1.0, '4010', 100000.00, 100000.00, 0)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    tx.commit().await.unwrap();

    // When: 仕訳から日次残高を更新
    let service = BalanceService::new(db.pool.clone());
    service
        .update_balance_from_journal_items(journal_no)
        .await
        .unwrap();

    // Then: 日次残高が正しく作成されている
    let results: Vec<(String, Decimal, Decimal)> = sqlx::query_as(
        r#"
        SELECT "勘定科目コード", "借方金額", "貸方金額"
        FROM "日次勘定科目残高"
        WHERE "起票日" = $1
        ORDER BY "勘定科目コード"
        "#,
    )
    .bind(journal_date)
    .fetch_all(&db.pool)
    .await
    .unwrap();

    assert_eq!(results.len(), 2);

    // 現金（借方）
    assert_eq!(results[0].0, "1010");
    assert_eq!(results[0].1, dec!(100000.00));
    assert_eq!(results[0].2, dec!(0.00));

    // 売上高（貸方）
    assert_eq!(results[1].0, "4010");
    assert_eq!(results[1].1, dec!(0.00));
    assert_eq!(results[1].2, dec!(100000.00));
}
