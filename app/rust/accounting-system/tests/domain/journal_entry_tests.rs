use crate::common::TestDatabase;
use chrono::NaiveDate;
use rust_decimal_macros::dec;
use sqlx::Row;

/// テスト用勘定科目を登録するヘルパー関数
async fn insert_test_accounts(db: &TestDatabase) {
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
        VALUES
        ('1100', '現金', '資産', false, true, 0),
        ('1200', '普通預金', '資産', false, true, 0),
        ('1300', '売掛金', '資産', false, true, 0),
        ('2120', '仮受消費税', '負債', false, true, 0),
        ('4100', '売上', '収益', false, true, 0),
        ('6200', '支払手数料', '費用', false, true, 0)
        "#,
    )
    .execute(&db.pool)
    .await
    .unwrap();
}

#[tokio::test]
async fn test_insert_journal_entry() {
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    // 1. 仕訳エントリを作成
    let voucher_no = sqlx::query_scalar::<_, String>(
        r#"
        INSERT INTO "仕訳エントリ" ("伝票番号", "仕訳日", "摘要", "合計金額", "作成者")
        VALUES ($1, $2, $3, $4, $5)
        RETURNING "伝票番号"
        "#,
    )
    .bind("JE240001")
    .bind(NaiveDate::from_ymd_opt(2024, 1, 15).unwrap())
    .bind("現金売上")
    .bind(dec!(110000.00))
    .bind("user001")
    .fetch_one(&db.pool)
    .await
    .unwrap();

    assert_eq!(voucher_no, "JE240001");

    // 2. 仕訳明細を作成（借方：現金、貸方：売上+消費税）

    // 借方：現金 110,000
    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "伝票番号", "行番号", "勘定科目コード",
            "借方金額", "貸方金額", "摘要"
        ) VALUES ($1, $2, $3, $4, $5, $6)
        "#,
    )
    .bind(&voucher_no)
    .bind(1)
    .bind("1100") // 現金
    .bind(dec!(110000.00))
    .bind(dec!(0))
    .bind("商品売上による現金収入")
    .execute(&db.pool)
    .await
    .unwrap();

    // 貸方：売上 100,000
    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "伝票番号", "行番号", "勘定科目コード",
            "借方金額", "貸方金額", "摘要"
        ) VALUES ($1, $2, $3, $4, $5, $6)
        "#,
    )
    .bind(&voucher_no)
    .bind(2)
    .bind("4100") // 売上
    .bind(dec!(0))
    .bind(dec!(100000.00))
    .bind("商品売上")
    .execute(&db.pool)
    .await
    .unwrap();

    // 貸方：仮受消費税 10,000
    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "伝票番号", "行番号", "勘定科目コード",
            "借方金額", "貸方金額", "摘要"
        ) VALUES ($1, $2, $3, $4, $5, $6)
        "#,
    )
    .bind(&voucher_no)
    .bind(3)
    .bind("2120") // 仮受消費税
    .bind(dec!(0))
    .bind(dec!(10000.00))
    .bind("消費税")
    .execute(&db.pool)
    .await
    .unwrap();

    // 3. 借方・貸方の合計を検証（複式簿記の原理）
    let row = sqlx::query(
        r#"
        SELECT
            SUM("借方金額") as debit_total,
            SUM("貸方金額") as credit_total
        FROM "仕訳明細"
        WHERE "伝票番号" = $1
        "#,
    )
    .bind(&voucher_no)
    .fetch_one(&db.pool)
    .await
    .unwrap();

    let debit_total: rust_decimal::Decimal = row.get("debit_total");
    let credit_total: rust_decimal::Decimal = row.get("credit_total");

    // 複式簿記の原理：借方合計 = 貸方合計
    assert_eq!(debit_total, credit_total);
    assert_eq!(debit_total, dec!(110000.00));
}

#[tokio::test]
async fn test_update_journal_entry() {
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    // 1. テストデータを登録
    sqlx::query(
        r#"
        INSERT INTO "仕訳エントリ" ("伝票番号", "仕訳日", "摘要", "合計金額", "作成者")
        VALUES ($1, $2, $3, $4, $5)
        "#,
    )
    .bind("JE240001")
    .bind(NaiveDate::from_ymd_opt(2024, 1, 15).unwrap())
    .bind("現金売上")
    .bind(dec!(110000.00))
    .bind("user001")
    .execute(&db.pool)
    .await
    .unwrap();

    // 2. 摘要を更新
    let updated = sqlx::query(
        r#"
        UPDATE "仕訳エントリ"
        SET "摘要" = $1, "更新者" = $2, "更新日時" = CURRENT_TIMESTAMP
        WHERE "伝票番号" = $3
        "#,
    )
    .bind("現金売上（修正）")
    .bind("user002")
    .bind("JE240001")
    .execute(&db.pool)
    .await
    .unwrap();

    assert_eq!(updated.rows_affected(), 1);

    // 3. 更新されたか検証
    let row = sqlx::query(
        r#"
        SELECT "摘要", "更新者"
        FROM "仕訳エントリ"
        WHERE "伝票番号" = $1
        "#,
    )
    .bind("JE240001")
    .fetch_one(&db.pool)
    .await
    .unwrap();

    assert_eq!(row.get::<String, _>("摘要"), "現金売上（修正）");
    assert_eq!(
        row.get::<Option<String>, _>("更新者"),
        Some("user002".to_string())
    );
}

#[tokio::test]
async fn test_delete_journal_entry_cascade() {
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    // 1. テストデータを登録
    sqlx::query(
        r#"
        INSERT INTO "仕訳エントリ" ("伝票番号", "仕訳日", "摘要", "合計金額", "作成者")
        VALUES ($1, $2, $3, $4, $5)
        "#,
    )
    .bind("JE240001")
    .bind(NaiveDate::from_ymd_opt(2024, 1, 15).unwrap())
    .bind("現金売上")
    .bind(dec!(110000.00))
    .bind("user001")
    .execute(&db.pool)
    .await
    .unwrap();

    // 仕訳明細も登録
    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "伝票番号", "行番号", "勘定科目コード",
            "借方金額", "貸方金額", "摘要"
        ) VALUES ($1, $2, $3, $4, $5, $6)
        "#,
    )
    .bind("JE240001")
    .bind(1)
    .bind("1100")
    .bind(dec!(110000))
    .bind(dec!(0))
    .bind("現金")
    .execute(&db.pool)
    .await
    .unwrap();

    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "伝票番号", "行番号", "勘定科目コード",
            "借方金額", "貸方金額", "摘要"
        ) VALUES ($1, $2, $3, $4, $5, $6)
        "#,
    )
    .bind("JE240001")
    .bind(2)
    .bind("4100")
    .bind(dec!(0))
    .bind(dec!(110000))
    .bind("売上")
    .execute(&db.pool)
    .await
    .unwrap();

    // 2. 仕訳エントリを削除
    let deleted = sqlx::query(r#"DELETE FROM "仕訳エントリ" WHERE "伝票番号" = $1"#)
        .bind("JE240001")
        .execute(&db.pool)
        .await
        .unwrap();

    assert_eq!(deleted.rows_affected(), 1);

    // 3. データが削除されたか検証
    let entry_count = sqlx::query_scalar::<_, i64>(
        r#"SELECT COUNT(*) FROM "仕訳エントリ" WHERE "伝票番号" = $1"#,
    )
    .bind("JE240001")
    .fetch_one(&db.pool)
    .await
    .unwrap();

    assert_eq!(entry_count, 0);

    // 明細も連鎖削除されているか確認
    let detail_count = sqlx::query_scalar::<_, i64>(
        r#"SELECT COUNT(*) FROM "仕訳明細" WHERE "伝票番号" = $1"#,
    )
    .bind("JE240001")
    .fetch_one(&db.pool)
    .await
    .unwrap();

    assert_eq!(detail_count, 0);
}

#[tokio::test]
async fn test_complex_journal_entry() {
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    // 1. 仕訳エントリを作成
    sqlx::query(
        r#"
        INSERT INTO "仕訳エントリ" ("伝票番号", "仕訳日", "摘要", "合計金額", "作成者")
        VALUES ($1, $2, $3, $4, $5)
        "#,
    )
    .bind("JE240002")
    .bind(NaiveDate::from_ymd_opt(2024, 1, 20).unwrap())
    .bind("売掛金回収と振込手数料")
    .bind(dec!(105000.00))
    .bind("user001")
    .execute(&db.pool)
    .await
    .unwrap();

    // 2. 複雑な仕訳明細を作成
    // 借方：普通預金 104,500（振込手数料差引後）
    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "伝票番号", "行番号", "勘定科目コード",
            "借方金額", "貸方金額", "摘要"
        ) VALUES ($1, $2, $3, $4, $5, $6)
        "#,
    )
    .bind("JE240002")
    .bind(1)
    .bind("1200")
    .bind(dec!(104500.00))
    .bind(dec!(0))
    .bind("売掛金回収（振込手数料差引後）")
    .execute(&db.pool)
    .await
    .unwrap();

    // 借方：支払手数料 500
    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "伝票番号", "行番号", "勘定科目コード",
            "借方金額", "貸方金額", "摘要"
        ) VALUES ($1, $2, $3, $4, $5, $6)
        "#,
    )
    .bind("JE240002")
    .bind(2)
    .bind("6200")
    .bind(dec!(500.00))
    .bind(dec!(0))
    .bind("振込手数料")
    .execute(&db.pool)
    .await
    .unwrap();

    // 貸方：売掛金 105,000
    sqlx::query(
        r#"
        INSERT INTO "仕訳明細" (
            "伝票番号", "行番号", "勘定科目コード",
            "借方金額", "貸方金額", "摘要"
        ) VALUES ($1, $2, $3, $4, $5, $6)
        "#,
    )
    .bind("JE240002")
    .bind(3)
    .bind("1300")
    .bind(dec!(0))
    .bind(dec!(105000.00))
    .bind("売掛金回収")
    .execute(&db.pool)
    .await
    .unwrap();

    // 3. 借方・貸方の合計を検証
    let row = sqlx::query(
        r#"
        SELECT
            SUM("借方金額") as debit_total,
            SUM("貸方金額") as credit_total
        FROM "仕訳明細"
        WHERE "伝票番号" = $1
        "#,
    )
    .bind("JE240002")
    .fetch_one(&db.pool)
    .await
    .unwrap();

    let debit_total: rust_decimal::Decimal = row.get("debit_total");
    let credit_total: rust_decimal::Decimal = row.get("credit_total");

    // 複式簿記の原理：借方合計 = 貸方合計
    assert_eq!(debit_total, credit_total);
    assert_eq!(debit_total, dec!(105000.00));
}
