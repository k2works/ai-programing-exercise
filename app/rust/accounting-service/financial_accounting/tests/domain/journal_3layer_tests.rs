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
        ('1010', '現金', '資産', false, true, 0),
        ('1020', '普通預金', '資産', false, true, 0),
        ('1130', '売掛金', '資産', false, true, 0),
        ('5110', '仕入高', '費用', false, true, 0),
        ('5410', '支払手数料', '費用', false, true, 0)
        ON CONFLICT ("勘定科目コード") DO NOTHING
        "#,
    )
    .execute(&db.pool)
    .await
    .unwrap();
}

#[tokio::test]
async fn test_journal_3layer_simple_entry() {
    // Given: 現金100,000円で商品を仕入れる仕訳
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let journal_no = "JE-20250101-001";
    let journal_date = NaiveDate::from_ymd_opt(2025, 1, 1).unwrap();
    let input_date = NaiveDate::from_ymd_opt(2025, 1, 1).unwrap();

    // When: 仕訳を登録（トランザクション内で実行）
    let mut tx = db.pool.begin().await.unwrap();

    // 1. 仕訳ヘッダー
    sqlx::query(
        r#"
        INSERT INTO "仕訳" (
            "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
            "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES ($1, $2, $3, 0, 1, 0, 0, 0)
        "#,
    )
    .bind(journal_no)
    .bind(journal_date)
    .bind(input_date)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 2. 仕訳明細（1行）
    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "仕訳伝票番号", "仕訳行番号", "行摘要"
        ) VALUES ($1, 1, '商品仕入')
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 3. 仕訳貸借明細（借方：仕入、貸方：現金）
    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
            "通貨コード", "為替レート", "勘定科目コード",
            "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 1, 'D', 'JPY', 1.0000, '5110', 100000.00, 100000.00, 0)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
            "通貨コード", "為替レート", "勘定科目コード",
            "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 1, 'C', 'JPY', 1.0000, '1010', 100000.00, 100000.00, 0)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    tx.commit().await.unwrap();

    // Then: データが正しく登録されていることを確認
    // 1. 仕訳が登録されている
    let journal_count: i64 =
        sqlx::query_scalar(r#"SELECT COUNT(*) FROM "仕訳" WHERE "仕訳伝票番号" = $1"#)
            .bind(journal_no)
            .fetch_one(&db.pool)
            .await
            .unwrap();
    assert_eq!(journal_count, 1);

    // 2. 仕訳明細が登録されている
    let detail_count: i64 =
        sqlx::query_scalar(r#"SELECT COUNT(*) FROM "仕訳明細" WHERE "仕訳伝票番号" = $1"#)
            .bind(journal_no)
            .fetch_one(&db.pool)
            .await
            .unwrap();
    assert_eq!(detail_count, 1);

    // 3. 仕訳貸借明細が2件（借方・貸方）登録されている
    let item_count: i64 =
        sqlx::query_scalar(r#"SELECT COUNT(*) FROM "仕訳貸借明細" WHERE "仕訳伝票番号" = $1"#)
            .bind(journal_no)
            .fetch_one(&db.pool)
            .await
            .unwrap();
    assert_eq!(item_count, 2);

    // 4. 借方・貸方の合計が一致する（複式簿記の原理）
    let result: (Decimal, Decimal) = sqlx::query_as(
        r#"
        SELECT
            SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END) as debit_total,
            SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END) as credit_total
        FROM "仕訳貸借明細"
        WHERE "仕訳伝票番号" = $1
        "#,
    )
    .bind(journal_no)
    .fetch_one(&db.pool)
    .await
    .unwrap();

    let (debit_total, credit_total) = result;
    assert_eq!(debit_total, credit_total);
    assert_eq!(debit_total, dec!(100000.00));
}

#[tokio::test]
async fn test_journal_3layer_compound_entry() {
    // Given: 売掛金の回収（振込手数料差引）
    // 売掛金 100,000円 → 普通預金 99,560円 + 支払手数料 440円
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let journal_no = "JE-20250102-001";
    let journal_date = NaiveDate::from_ymd_opt(2025, 1, 2).unwrap();

    // When: 仕訳を登録（トランザクション内で実行）
    let mut tx = db.pool.begin().await.unwrap();

    // 1. 仕訳ヘッダー（単振フラグ = 0: 複合仕訳）
    sqlx::query(
        r#"
        INSERT INTO "仕訳" (
            "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
            "仕訳伝票区分", "定期計上フラグ", "赤伝フラグ"
        ) VALUES ($1, $2, $2, 0, 0, 0, 0, 0)
        "#,
    )
    .bind(journal_no)
    .bind(journal_date)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 2. 仕訳明細（2行）
    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "仕訳伝票番号", "仕訳行番号", "行摘要"
        ) VALUES ($1, 1, '売掛金回収（A社）')
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "仕訳伝票番号", "仕訳行番号", "行摘要"
        ) VALUES ($1, 2, '振込手数料')
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 3. 仕訳貸借明細
    // 行1-借方: 普通預金 99,560円
    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
            "通貨コード", "為替レート", "勘定科目コード",
            "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 1, 'D', 'JPY', 1.0000, '1020', 99560.00, 99560.00, 1)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 行1-貸方: 売掛金 99,560円
    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
            "通貨コード", "為替レート", "勘定科目コード",
            "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 1, 'C', 'JPY', 1.0000, '1130', 99560.00, 99560.00, 0)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 行2-借方: 支払手数料 440円
    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
            "通貨コード", "為替レート", "勘定科目コード",
            "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 2, 'D', 'JPY', 1.0000, '5410', 440.00, 440.00, 0)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 行2-貸方: 売掛金 440円
    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
            "通貨コード", "為替レート", "勘定科目コード",
            "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 2, 'C', 'JPY', 1.0000, '1130', 440.00, 440.00, 0)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    tx.commit().await.unwrap();

    // Then: データが正しく登録されていることを確認
    // 1. 仕訳明細が2件登録されている
    let detail_count: i64 =
        sqlx::query_scalar(r#"SELECT COUNT(*) FROM "仕訳明細" WHERE "仕訳伝票番号" = $1"#)
            .bind(journal_no)
            .fetch_one(&db.pool)
            .await
            .unwrap();
    assert_eq!(detail_count, 2);

    // 2. 仕訳貸借明細が4件登録されている
    let item_count: i64 =
        sqlx::query_scalar(r#"SELECT COUNT(*) FROM "仕訳貸借明細" WHERE "仕訳伝票番号" = $1"#)
            .bind(journal_no)
            .fetch_one(&db.pool)
            .await
            .unwrap();
    assert_eq!(item_count, 4);

    // 3. 借方・貸方の合計が一致する
    let result: (Decimal, Decimal) = sqlx::query_as(
        r#"
        SELECT
            SUM(CASE WHEN "仕訳行貸借区分" = 'D' THEN "仕訳金額" ELSE 0 END) as debit_total,
            SUM(CASE WHEN "仕訳行貸借区分" = 'C' THEN "仕訳金額" ELSE 0 END) as credit_total
        FROM "仕訳貸借明細"
        WHERE "仕訳伝票番号" = $1
        "#,
    )
    .bind(journal_no)
    .fetch_one(&db.pool)
    .await
    .unwrap();

    let (debit_total, credit_total) = result;
    assert_eq!(debit_total, credit_total);
    assert_eq!(debit_total, dec!(100000.00));

    // 4. 単振フラグが0（複合仕訳）になっている
    let tanpu_flag: i32 =
        sqlx::query_scalar(r#"SELECT "単振フラグ" FROM "仕訳" WHERE "仕訳伝票番号" = $1"#)
            .bind(journal_no)
            .fetch_one(&db.pool)
            .await
            .unwrap();
    assert_eq!(tanpu_flag, 0);
}

#[tokio::test]
async fn test_journal_cascade_delete() {
    // Given: 仕訳を登録
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let journal_no = "JE-20250103-001";
    let journal_date = NaiveDate::from_ymd_opt(2025, 1, 3).unwrap();

    // トランザクション内で仕訳を登録
    let mut tx = db.pool.begin().await.unwrap();

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

    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "仕訳伝票番号", "仕訳行番号", "行摘要"
        ) VALUES ($1, 1, 'テスト')
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 借方
    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
            "通貨コード", "為替レート", "勘定科目コード",
            "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 1, 'D', 'JPY', 1.0000, '5110', 10000.00, 10000.00, 0)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 貸方（借方と一致させる）
    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
            "通貨コード", "為替レート", "勘定科目コード",
            "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 1, 'C', 'JPY', 1.0000, '1010', 10000.00, 10000.00, 0)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    tx.commit().await.unwrap();

    // When: 仕訳を削除
    sqlx::query(r#"DELETE FROM "仕訳" WHERE "仕訳伝票番号" = $1"#)
        .bind(journal_no)
        .execute(&db.pool)
        .await
        .unwrap();

    // Then: 明細と貸借明細も自動削除される（CASCADE）
    let detail_count: i64 =
        sqlx::query_scalar(r#"SELECT COUNT(*) FROM "仕訳明細" WHERE "仕訳伝票番号" = $1"#)
            .bind(journal_no)
            .fetch_one(&db.pool)
            .await
            .unwrap();
    assert_eq!(detail_count, 0);

    let item_count: i64 =
        sqlx::query_scalar(r#"SELECT COUNT(*) FROM "仕訳貸借明細" WHERE "仕訳伝票番号" = $1"#)
            .bind(journal_no)
            .fetch_one(&db.pool)
            .await
            .unwrap();
    assert_eq!(item_count, 0);
}
