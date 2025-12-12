mod common;

use chrono::{Datelike, NaiveDate, Utc};

use accounting_system::infrastructure::cqrs::journal_read_model::*;
use common::TestDatabase;

#[tokio::test]
async fn test_create_journal_read_model() {
    // Given: 読み取りモデルリポジトリ
    let db = TestDatabase::new().await;
    let repo = JournalReadModelRepository::new(db.pool.clone());

    // When: 読み取りモデルを作成
    let read_model = JournalReadModel {
        journal_id: "J-001".to_string(),
        journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
        description: "売掛金の回収".to_string(),
        fiscal_year: 2024,
        status: "draft".to_string(),
        total_debit: 100000,
        total_credit: 100000,
        entry_count: 2,
        approved_by: None,
        approved_at: None,
        created_at: Utc::now(),
        updated_at: Utc::now(),
    };

    repo.upsert(&read_model).await.unwrap();

    // Then: 読み取りモデルが保存される
    let retrieved = repo.find_by_id("J-001").await.unwrap();
    assert!(retrieved.is_some());
    assert_eq!(retrieved.unwrap().description, "売掛金の回収");
}

#[tokio::test]
async fn test_upsert_journal_read_model() {
    // Given: 既存の読み取りモデル
    let db = TestDatabase::new().await;
    let repo = JournalReadModelRepository::new(db.pool.clone());

    let mut read_model = JournalReadModel {
        journal_id: "J-002".to_string(),
        journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
        description: "最初の説明".to_string(),
        fiscal_year: 2024,
        status: "draft".to_string(),
        total_debit: 100000,
        total_credit: 100000,
        entry_count: 2,
        approved_by: None,
        approved_at: None,
        created_at: Utc::now(),
        updated_at: Utc::now(),
    };

    repo.upsert(&read_model).await.unwrap();

    // When: 同じIDで更新
    read_model.description = "更新された説明".to_string();
    read_model.status = "approved".to_string();
    read_model.updated_at = Utc::now();

    repo.upsert(&read_model).await.unwrap();

    // Then: データが更新される
    let retrieved = repo.find_by_id("J-002").await.unwrap().unwrap();
    assert_eq!(retrieved.description, "更新された説明");
    assert_eq!(retrieved.status, "approved");
}

#[tokio::test]
async fn test_list_journals_by_status() {
    // Given: 複数の仕訳が存在
    let db = TestDatabase::new().await;
    let repo = JournalReadModelRepository::new(db.pool.clone());

    // 下書き仕訳
    let draft1 = create_test_read_model("J-003", "draft");
    repo.upsert(&draft1).await.unwrap();

    let draft2 = create_test_read_model("J-004", "draft");
    repo.upsert(&draft2).await.unwrap();

    // 承認済み仕訳
    let approved = create_test_read_model("J-005", "approved");
    repo.upsert(&approved).await.unwrap();

    // When: ステータスで検索
    let drafts = repo.list_by_status("draft", 10, 0).await.unwrap();

    // Then: 下書き仕訳のみ取得される
    assert_eq!(drafts.len(), 2);
    assert!(drafts.iter().all(|j| j.status == "draft"));
}

#[tokio::test]
async fn test_search_journals_by_date_range() {
    // Given: 複数の期間の仕訳が存在
    let db = TestDatabase::new().await;
    let repo = JournalReadModelRepository::new(db.pool.clone());

    let mut journal1 = create_test_read_model("J-006", "approved");
    journal1.journal_date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
    repo.upsert(&journal1).await.unwrap();

    let mut journal2 = create_test_read_model("J-007", "approved");
    journal2.journal_date = NaiveDate::from_ymd_opt(2024, 2, 20).unwrap();
    repo.upsert(&journal2).await.unwrap();

    let mut journal3 = create_test_read_model("J-008", "approved");
    journal3.journal_date = NaiveDate::from_ymd_opt(2024, 1, 25).unwrap();
    repo.upsert(&journal3).await.unwrap();

    // When: 日付範囲で検索（1月のみ）
    let results = repo
        .find_by_date_range(
            NaiveDate::from_ymd_opt(2024, 1, 1).unwrap(),
            NaiveDate::from_ymd_opt(2024, 1, 31).unwrap(),
        )
        .await
        .unwrap();

    // Then: 期間内の仕訳のみ取得される
    assert_eq!(results.len(), 2);
    assert!(results.iter().all(|j| j.journal_date.month() == 1));
}

#[tokio::test]
async fn test_add_and_get_entries() {
    // Given: 仕訳の読み取りモデルが存在
    let db = TestDatabase::new().await;
    let repo = JournalReadModelRepository::new(db.pool.clone());

    let journal = create_test_read_model("J-009", "draft");
    repo.upsert(&journal).await.unwrap();

    // When: 明細を追加
    let entry1 = JournalEntryReadModel {
        entry_id: 0, // auto-generated
        journal_id: "J-009".to_string(),
        account_code: "1010".to_string(),
        account_name: Some("現金".to_string()),
        debit_amount: Some(100000),
        credit_amount: None,
        description: Some("借方明細".to_string()),
        line_number: 1,
        created_at: Utc::now(),
    };

    let entry2 = JournalEntryReadModel {
        entry_id: 0,
        journal_id: "J-009".to_string(),
        account_code: "4100".to_string(),
        account_name: Some("売上高".to_string()),
        debit_amount: None,
        credit_amount: Some(100000),
        description: Some("貸方明細".to_string()),
        line_number: 2,
        created_at: Utc::now(),
    };

    repo.add_entry(&entry1).await.unwrap();
    repo.add_entry(&entry2).await.unwrap();

    // Then: 明細が取得できる
    let entries = repo.get_entries("J-009").await.unwrap();
    assert_eq!(entries.len(), 2);
    assert_eq!(entries[0].account_code, "1010");
    assert_eq!(entries[0].debit_amount, Some(100000));
    assert_eq!(entries[1].account_code, "4100");
    assert_eq!(entries[1].credit_amount, Some(100000));
}

#[tokio::test]
async fn test_delete_entry() {
    // Given: 明細が存在
    let db = TestDatabase::new().await;
    let repo = JournalReadModelRepository::new(db.pool.clone());

    let journal = create_test_read_model("J-010", "draft");
    repo.upsert(&journal).await.unwrap();

    let entry = JournalEntryReadModel {
        entry_id: 0,
        journal_id: "J-010".to_string(),
        account_code: "1010".to_string(),
        account_name: Some("現金".to_string()),
        debit_amount: Some(100000),
        credit_amount: None,
        description: Some("削除テスト".to_string()),
        line_number: 1,
        created_at: Utc::now(),
    };

    let entry_id = repo.add_entry(&entry).await.unwrap();

    // When: 明細を削除
    repo.delete_entry(entry_id).await.unwrap();

    // Then: 明細が削除される
    let entries = repo.get_entries("J-010").await.unwrap();
    assert_eq!(entries.len(), 0);
}

// テスト用ヘルパー
fn create_test_read_model(journal_id: &str, status: &str) -> JournalReadModel {
    JournalReadModel {
        journal_id: journal_id.to_string(),
        journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
        description: format!("テスト仕訳 {}", journal_id),
        fiscal_year: 2024,
        status: status.to_string(),
        total_debit: 100000,
        total_credit: 100000,
        entry_count: 2,
        approved_by: None,
        approved_at: None,
        created_at: Utc::now(),
        updated_at: Utc::now(),
    }
}
