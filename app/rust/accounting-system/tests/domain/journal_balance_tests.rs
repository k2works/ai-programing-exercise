use crate::common::TestDatabase;
use chrono::NaiveDate;

/// テスト用勘定科目を登録するヘルパー関数
async fn insert_test_accounts(db: &TestDatabase) {
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
        VALUES
        ('1010', '現金', '資産', false, true, 0),
        ('5110', '仕入高', '費用', false, true, 0)
        ON CONFLICT ("勘定科目コード") DO NOTHING
        "#,
    )
    .execute(&db.pool)
    .await
    .unwrap();
}

#[tokio::test]
async fn test_journal_balance_valid() {
    // Given: テスト用データベース
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let journal_no = "JE-BAL-001";
    let journal_date = NaiveDate::from_ymd_opt(2025, 1, 1).unwrap();

    // When: 借方・貸方が一致する仕訳を登録（トランザクション内で実行）
    let mut tx = db.pool.begin().await.unwrap();

    // 1. 仕訳ヘッダー
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

    // 2. 仕訳明細
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

    // 3. 仕訳貸借明細（借方：仕入 100,000円）
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

    // 4. 仕訳貸借明細（貸方：現金 100,000円）
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

    // トランザクションをコミット
    let result = tx.commit().await;

    // Then: 借方・貸方が一致しているので正常に登録できる
    assert!(result.is_ok());

    // データが正しく登録されていることを確認
    let count: i64 = sqlx::query_scalar(
        r#"SELECT COUNT(*) FROM "仕訳貸借明細" WHERE "仕訳伝票番号" = $1"#,
    )
    .bind(journal_no)
    .fetch_one(&db.pool)
    .await
    .unwrap();
    assert_eq!(count, 2);
}

#[tokio::test]
async fn test_journal_balance_invalid() {
    // Given: テスト用データベース
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let journal_no = "JE-BAL-002";
    let journal_date = NaiveDate::from_ymd_opt(2025, 1, 2).unwrap();

    // When: 借方・貸方が一致しない仕訳を登録しようとする（トランザクション内で実行）
    let mut tx = db.pool.begin().await.unwrap();

    // 1. 仕訳ヘッダー
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

    // 2. 仕訳明細
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

    // 3. 仕訳貸借明細（借方：仕入 100,000円）
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

    // 4. 仕訳貸借明細（貸方：現金 99,000円）← 借方と一致しない
    sqlx::query(
        r#"
        INSERT INTO "仕訳貸借明細" (
            "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
            "通貨コード", "為替レート", "勘定科目コード",
            "仕訳金額", "基軸換算仕訳金額", "資金繰フラグ"
        ) VALUES ($1, 1, 'C', 'JPY', 1.0000, '1010', 99000.00, 99000.00, 0)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // トランザクションをコミット（ここでトリガーが発火してエラーになる）
    let result = tx.commit().await;

    // Then: トリガーにより借方・貸方が一致しないためエラーになる
    assert!(result.is_err());

    // エラーメッセージに「借方合計」「貸方合計」が含まれることを確認
    let error_message = result.unwrap_err().to_string();
    assert!(error_message.contains("借方合計") || error_message.contains("貸方合計"));
}

#[tokio::test]
async fn test_journal_balance_update() {
    // Given: 正常な仕訳が登録されている
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let journal_no = "JE-BAL-003";
    let journal_date = NaiveDate::from_ymd_opt(2025, 1, 3).unwrap();

    // トランザクション内で仕訳を登録
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
        ) VALUES ($1, 1, 'D', 'JPY', 1.0000, '5110', 100000.00, 100000.00, 0)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 貸方
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

    // When: 貸借を崩すような更新をしようとする（トランザクション内で実行）
    let mut tx = db.pool.begin().await.unwrap();

    sqlx::query(
        r#"
        UPDATE "仕訳貸借明細"
        SET "仕訳金額" = 50000.00, "基軸換算仕訳金額" = 50000.00
        WHERE "仕訳伝票番号" = $1 AND "仕訳行貸借区分" = 'C'
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    let result = tx.commit().await;

    // Then: トリガーによりエラーになる
    assert!(result.is_err());
}

#[tokio::test]
async fn test_journal_balance_delete() {
    // Given: 正常な仕訳が登録されている
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let journal_no = "JE-BAL-004";
    let journal_date = NaiveDate::from_ymd_opt(2025, 1, 4).unwrap();

    // トランザクション内で仕訳を登録
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
        ) VALUES ($1, 1, 'D', 'JPY', 1.0000, '5110', 100000.00, 100000.00, 0)
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    // 貸方
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

    // When: 貸借を崩すような削除をしようとする（トランザクション内で実行）
    let mut tx = db.pool.begin().await.unwrap();

    sqlx::query(
        r#"
        DELETE FROM "仕訳貸借明細"
        WHERE "仕訳伝票番号" = $1 AND "仕訳行貸借区分" = 'C'
        "#,
    )
    .bind(journal_no)
    .execute(&mut *tx)
    .await
    .unwrap();

    let result = tx.commit().await;

    // Then: トリガーによりエラーになる
    assert!(result.is_err());
}
