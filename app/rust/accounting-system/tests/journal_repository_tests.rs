mod common;

use accounting_system::domain::journal::{Journal, JournalDebitCreditItem, JournalDetail};
use accounting_system::repositories::journal::JournalRepository;
use chrono::NaiveDate;
use common::TestDatabase;
use rust_decimal_macros::dec;

/// テスト用勘定科目を登録するヘルパー関数
async fn insert_test_accounts(db: &TestDatabase) {
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "合計科目", "集計対象", "残高")
        VALUES
        ('1010', '現金', '資産', false, true, 0),
        ('4100', '売上', '収益', false, true, 0),
        ('5110', '仕入高', '費用', false, true, 0)
        ON CONFLICT ("勘定科目コード") DO NOTHING
        "#,
    )
    .execute(&db.pool)
    .await
    .unwrap();
}

#[tokio::test]
async fn test_journal_repository_save_and_find() {
    // Given: テスト用データベース
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let repo = JournalRepository::new(&db.pool);

    // 仕訳を作成
    let mut journal = Journal::new(
        "JE-REPO-001".to_string(),
        NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
        NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
        0,
        1,
    );

    let mut detail = JournalDetail::new("JE-REPO-001".to_string(), 1, "商品販売".to_string());

    detail.add_item(JournalDebitCreditItem::new(
        "JE-REPO-001".to_string(),
        1,
        "D".to_string(),
        "1010".to_string(),
        dec!(100000),
    ));

    detail.add_item(JournalDebitCreditItem::new(
        "JE-REPO-001".to_string(),
        1,
        "C".to_string(),
        "4100".to_string(),
        dec!(100000),
    ));

    journal.add_detail(detail);

    // バランスを検証
    assert!(journal.validate_balance().is_ok());

    // When: 仕訳を保存
    repo.save(&journal).await.unwrap();

    // Then: 仕訳を検索できる
    let found = repo.find_by_journal_no("JE-REPO-001").await.unwrap();
    assert!(found.is_some());

    let found_journal = found.unwrap();
    assert_eq!(found_journal.journal_no, "JE-REPO-001");
    assert_eq!(found_journal.details.len(), 1);
    assert_eq!(found_journal.details[0].items.len(), 2);
    assert_eq!(found_journal.calculate_debit_total(), dec!(100000));
    assert_eq!(found_journal.calculate_credit_total(), dec!(100000));
}

#[tokio::test]
async fn test_journal_repository_find_by_date_range() {
    // Given: テスト用データベース
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let repo = JournalRepository::new(&db.pool);

    // 3つの仕訳を作成（異なる日付）
    for i in 1..=3 {
        let mut journal = Journal::new(
            format!("JE-DATE-{:03}", i),
            NaiveDate::from_ymd_opt(2025, 1, i).unwrap(),
            NaiveDate::from_ymd_opt(2025, 1, i).unwrap(),
            0,
            1,
        );

        let mut detail = JournalDetail::new(format!("JE-DATE-{:03}", i), 1, format!("テスト{}", i));

        detail.add_item(JournalDebitCreditItem::new(
            format!("JE-DATE-{:03}", i),
            1,
            "D".to_string(),
            "5110".to_string(),
            dec!(10000) * rust_decimal::Decimal::from(i),
        ));

        detail.add_item(JournalDebitCreditItem::new(
            format!("JE-DATE-{:03}", i),
            1,
            "C".to_string(),
            "1010".to_string(),
            dec!(10000) * rust_decimal::Decimal::from(i),
        ));

        journal.add_detail(detail);
        repo.save(&journal).await.unwrap();
    }

    // When: 日付範囲で検索
    let journals = repo
        .find_by_date_range(
            NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            NaiveDate::from_ymd_opt(2025, 1, 2).unwrap(),
        )
        .await
        .unwrap();

    // Then: 2件取得できる
    assert_eq!(journals.len(), 2);
    assert_eq!(journals[0].journal_no, "JE-DATE-001");
    assert_eq!(journals[1].journal_no, "JE-DATE-002");
}

#[tokio::test]
async fn test_journal_repository_delete() {
    // Given: 保存された仕訳
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let repo = JournalRepository::new(&db.pool);

    let mut journal = Journal::new(
        "JE-DELETE-001".to_string(),
        NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
        NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
        0,
        1,
    );

    let mut detail = JournalDetail::new("JE-DELETE-001".to_string(), 1, "削除テスト".to_string());

    detail.add_item(JournalDebitCreditItem::new(
        "JE-DELETE-001".to_string(),
        1,
        "D".to_string(),
        "1010".to_string(),
        dec!(50000),
    ));

    detail.add_item(JournalDebitCreditItem::new(
        "JE-DELETE-001".to_string(),
        1,
        "C".to_string(),
        "4100".to_string(),
        dec!(50000),
    ));

    journal.add_detail(detail);
    repo.save(&journal).await.unwrap();

    // When: 仕訳を削除
    repo.delete("JE-DELETE-001").await.unwrap();

    // Then: 仕訳が見つからない
    let found = repo.find_by_journal_no("JE-DELETE-001").await.unwrap();
    assert!(found.is_none());
}

#[tokio::test]
async fn test_journal_repository_compound_entry() {
    // Given: テスト用データベース
    let db = TestDatabase::new().await;
    insert_test_accounts(&db).await;

    let repo = JournalRepository::new(&db.pool);

    // 複合仕訳を作成（2行）
    let mut journal = Journal::new(
        "JE-COMPOUND-001".to_string(),
        NaiveDate::from_ymd_opt(2025, 1, 5).unwrap(),
        NaiveDate::from_ymd_opt(2025, 1, 5).unwrap(),
        0,
        0, // 複合仕訳
    );

    // 行1: 現金（借方）と売上（貸方）
    let mut detail1 = JournalDetail::new("JE-COMPOUND-001".to_string(), 1, "現金売上".to_string());

    detail1.add_item(JournalDebitCreditItem::new(
        "JE-COMPOUND-001".to_string(),
        1,
        "D".to_string(),
        "1010".to_string(),
        dec!(70000),
    ));

    detail1.add_item(JournalDebitCreditItem::new(
        "JE-COMPOUND-001".to_string(),
        1,
        "C".to_string(),
        "4100".to_string(),
        dec!(70000),
    ));

    journal.add_detail(detail1);

    // 行2: 仕入（借方）と現金（貸方）
    let mut detail2 = JournalDetail::new("JE-COMPOUND-001".to_string(), 2, "商品仕入".to_string());

    detail2.add_item(JournalDebitCreditItem::new(
        "JE-COMPOUND-001".to_string(),
        2,
        "D".to_string(),
        "5110".to_string(),
        dec!(30000),
    ));

    detail2.add_item(JournalDebitCreditItem::new(
        "JE-COMPOUND-001".to_string(),
        2,
        "C".to_string(),
        "1010".to_string(),
        dec!(30000),
    ));

    journal.add_detail(detail2);

    // バランスを検証
    assert!(journal.validate_balance().is_ok());
    assert_eq!(journal.calculate_debit_total(), dec!(100000));
    assert_eq!(journal.calculate_credit_total(), dec!(100000));

    // When: 仕訳を保存
    repo.save(&journal).await.unwrap();

    // Then: 仕訳を検索できる
    let found = repo.find_by_journal_no("JE-COMPOUND-001").await.unwrap();
    assert!(found.is_some());

    let found_journal = found.unwrap();
    assert_eq!(found_journal.journal_no, "JE-COMPOUND-001");
    assert_eq!(found_journal.single_entry_flag, 0); // 複合仕訳
    assert_eq!(found_journal.details.len(), 2);
    assert_eq!(found_journal.calculate_debit_total(), dec!(100000));
    assert_eq!(found_journal.calculate_credit_total(), dec!(100000));
}
