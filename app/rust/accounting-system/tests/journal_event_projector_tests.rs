mod common;

use chrono::{NaiveDate, Utc};
use rust_decimal_macros::dec;

use accounting_system::domain::events::journal_events::*;
use accounting_system::infrastructure::cqrs::journal_event_projector::*;
use accounting_system::infrastructure::cqrs::journal_read_model::*;
use common::TestDatabase;

#[tokio::test]
async fn test_project_journal_created_event() {
    // Given: イベントプロジェクター
    let db = TestDatabase::new().await;
    let projector = JournalEventProjector::new(db.pool.clone());

    // When: JournalCreated イベントを処理
    let event = JournalEvent::Created(JournalCreatedData {
        journal_id: "J-101".to_string(),
        journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
        description: "売掛金の回収".to_string(),
    });

    projector.handle("J-101", &event).await.unwrap();

    // Then: 読み取りモデルが作成される
    let repo = JournalReadModelRepository::new(db.pool);
    let read_model = repo.find_by_id("J-101").await.unwrap();
    assert!(read_model.is_some());
    let model = read_model.unwrap();
    assert_eq!(model.status, "draft");
    assert_eq!(model.entry_count, 0);
    assert_eq!(model.fiscal_year, 2024);
}

#[tokio::test]
async fn test_project_entry_added_event() {
    // Given: 仕訳の読み取りモデルが存在
    let db = TestDatabase::new().await;
    let projector = JournalEventProjector::new(db.pool.clone());

    // 仕訳作成
    let created_event = JournalEvent::Created(JournalCreatedData {
        journal_id: "J-102".to_string(),
        journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
        description: "テスト仕訳".to_string(),
    });
    projector.handle("J-102", &created_event).await.unwrap();

    // When: EntryAdded イベントを処理
    let event = JournalEvent::EntryAdded(EntryAddedData {
        entry_id: 1,
        account_code: "1110".to_string(),
        debit_credit: "D".to_string(),
        amount: dec!(100000),
        description: "普通預金".to_string(),
    });

    projector.handle("J-102", &event).await.unwrap();

    // Then: 読み取りモデルが更新される
    let repo = JournalReadModelRepository::new(db.pool.clone());
    let model = repo.find_by_id("J-102").await.unwrap().unwrap();
    assert_eq!(model.entry_count, 1);
    assert_eq!(model.total_debit, 10000000); // 100000 * 100

    // 明細も作成される
    let entries = repo.get_entries("J-102").await.unwrap();
    assert_eq!(entries.len(), 1);
    assert_eq!(entries[0].account_code, "1110");
    assert_eq!(entries[0].debit_amount, Some(10000000));
}

#[tokio::test]
async fn test_project_multiple_entries() {
    // Given: 仕訳の読み取りモデルが存在
    let db = TestDatabase::new().await;
    let projector = JournalEventProjector::new(db.pool.clone());

    // 仕訳作成
    let created_event = JournalEvent::Created(JournalCreatedData {
        journal_id: "J-103".to_string(),
        journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
        description: "複合仕訳".to_string(),
    });
    projector.handle("J-103", &created_event).await.unwrap();

    // When: 複数の明細を追加
    let debit_event = JournalEvent::EntryAdded(EntryAddedData {
        entry_id: 1,
        account_code: "1010".to_string(),
        debit_credit: "D".to_string(),
        amount: dec!(100000),
        description: "現金".to_string(),
    });
    projector.handle("J-103", &debit_event).await.unwrap();

    let credit_event = JournalEvent::EntryAdded(EntryAddedData {
        entry_id: 2,
        account_code: "4100".to_string(),
        debit_credit: "C".to_string(),
        amount: dec!(100000),
        description: "売上高".to_string(),
    });
    projector.handle("J-103", &credit_event).await.unwrap();

    // Then: 読み取りモデルが正しく更新される
    let repo = JournalReadModelRepository::new(db.pool.clone());
    let model = repo.find_by_id("J-103").await.unwrap().unwrap();
    assert_eq!(model.entry_count, 2);
    assert_eq!(model.total_debit, 10000000);
    assert_eq!(model.total_credit, 10000000);
}

#[tokio::test]
async fn test_project_entry_removed_event() {
    // Given: 明細が存在する仕訳
    let db = TestDatabase::new().await;
    let projector = JournalEventProjector::new(db.pool.clone());

    // 仕訳作成と明細追加
    let created_event = JournalEvent::Created(JournalCreatedData {
        journal_id: "J-104".to_string(),
        journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
        description: "削除テスト".to_string(),
    });
    projector.handle("J-104", &created_event).await.unwrap();

    let entry_event = JournalEvent::EntryAdded(EntryAddedData {
        entry_id: 1,
        account_code: "1010".to_string(),
        debit_credit: "D".to_string(),
        amount: dec!(50000),
        description: "現金".to_string(),
    });
    projector.handle("J-104", &entry_event).await.unwrap();

    // When: 明細削除イベントを処理
    let removed_event = JournalEvent::EntryRemoved(EntryRemovedData { entry_id: 1 });
    projector.handle("J-104", &removed_event).await.unwrap();

    // Then: 明細が削除され、合計が更新される
    let repo = JournalReadModelRepository::new(db.pool.clone());
    let model = repo.find_by_id("J-104").await.unwrap().unwrap();
    assert_eq!(model.entry_count, 0);
    assert_eq!(model.total_debit, 0);

    let entries = repo.get_entries("J-104").await.unwrap();
    assert_eq!(entries.len(), 0);
}

#[tokio::test]
async fn test_project_approved_event() {
    // Given: 下書き仕訳が存在
    let db = TestDatabase::new().await;
    let projector = JournalEventProjector::new(db.pool.clone());

    // 仕訳作成
    let created_event = JournalEvent::Created(JournalCreatedData {
        journal_id: "J-105".to_string(),
        journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
        description: "承認テスト".to_string(),
    });
    projector.handle("J-105", &created_event).await.unwrap();

    // When: 承認イベントを処理
    let approved_at = Utc::now();
    let approved_event = JournalEvent::Approved(JournalApprovedData {
        approved_by: "manager@example.com".to_string(),
        approved_at,
    });
    projector.handle("J-105", &approved_event).await.unwrap();

    // Then: ステータスが承認済みに更新される
    let repo = JournalReadModelRepository::new(db.pool.clone());
    let model = repo.find_by_id("J-105").await.unwrap().unwrap();
    assert_eq!(model.status, "approved");
    assert_eq!(model.approved_by, Some("manager@example.com".to_string()));
    assert!(model.approved_at.is_some());
}

#[tokio::test]
async fn test_project_canceled_event() {
    // Given: 承認済み仕訳が存在
    let db = TestDatabase::new().await;
    let projector = JournalEventProjector::new(db.pool.clone());

    // 仕訳作成
    let created_event = JournalEvent::Created(JournalCreatedData {
        journal_id: "J-106".to_string(),
        journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
        description: "取消テスト".to_string(),
    });
    projector.handle("J-106", &created_event).await.unwrap();

    // 承認
    let approved_event = JournalEvent::Approved(JournalApprovedData {
        approved_by: "manager@example.com".to_string(),
        approved_at: Utc::now(),
    });
    projector.handle("J-106", &approved_event).await.unwrap();

    // When: 取消イベントを処理
    let canceled_event = JournalEvent::Canceled(JournalCanceledData {
        reason: "誤入力のため".to_string(),
        canceled_by: "admin@example.com".to_string(),
        canceled_at: Utc::now(),
    });
    projector.handle("J-106", &canceled_event).await.unwrap();

    // Then: ステータスが取消済みに更新される
    let repo = JournalReadModelRepository::new(db.pool.clone());
    let model = repo.find_by_id("J-106").await.unwrap().unwrap();
    assert_eq!(model.status, "canceled");
}

#[tokio::test]
async fn test_complete_journal_lifecycle() {
    // Given: イベントプロジェクター
    let db = TestDatabase::new().await;
    let projector = JournalEventProjector::new(db.pool.clone());

    // When: 仕訳の完全なライフサイクルをシミュレート
    // 1. 仕訳作成
    let created = JournalEvent::Created(JournalCreatedData {
        journal_id: "J-107".to_string(),
        journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
        description: "完全ライフサイクルテスト".to_string(),
    });
    projector.handle("J-107", &created).await.unwrap();

    // 2. 明細追加（借方）
    let debit = JournalEvent::EntryAdded(EntryAddedData {
        entry_id: 1,
        account_code: "1010".to_string(),
        debit_credit: "D".to_string(),
        amount: dec!(150000),
        description: "現金".to_string(),
    });
    projector.handle("J-107", &debit).await.unwrap();

    // 3. 明細追加（貸方）
    let credit = JournalEvent::EntryAdded(EntryAddedData {
        entry_id: 2,
        account_code: "4100".to_string(),
        debit_credit: "C".to_string(),
        amount: dec!(150000),
        description: "売上高".to_string(),
    });
    projector.handle("J-107", &credit).await.unwrap();

    // 4. 承認
    let approved = JournalEvent::Approved(JournalApprovedData {
        approved_by: "manager@example.com".to_string(),
        approved_at: Utc::now(),
    });
    projector.handle("J-107", &approved).await.unwrap();

    // Then: 最終状態を確認
    let repo = JournalReadModelRepository::new(db.pool.clone());
    let model = repo.find_by_id("J-107").await.unwrap().unwrap();

    assert_eq!(model.status, "approved");
    assert_eq!(model.entry_count, 2);
    assert_eq!(model.total_debit, 15000000); // 150000 * 100
    assert_eq!(model.total_credit, 15000000);
    assert!(model.approved_by.is_some());
}
