mod common;

use chrono::NaiveDate;
use rust_decimal_macros::dec;

use accounting_system::domain::aggregates::journal::*;
use accounting_system::domain::events::journal_events::*;
use accounting_system::infrastructure::event_sourcing::snapshot_repository::*;
use common::TestDatabase;

#[tokio::test]
async fn test_save_snapshot() {
    // Given: 仕訳集約
    let mut journal = JournalAggregate::new();
    journal.journal_id = "J-001".to_string();
    journal.journal_date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
    journal.description = "売掛金の回収".to_string();
    journal.status = JournalStatus::Approved;

    // When: スナップショットを保存
    let db = TestDatabase::new().await;
    let repo = SnapshotRepository::new(db.pool.clone());
    repo.save_snapshot("Journal", "J-001", 10, &journal)
        .await
        .unwrap();

    // Then: スナップショットが保存される
    let snapshot = repo
        .load_snapshot::<JournalAggregate>("Journal", "J-001")
        .await
        .unwrap();
    assert!(snapshot.is_some());
    let (version, saved_journal) = snapshot.unwrap();
    assert_eq!(version, 10);
    assert_eq!(saved_journal.journal_id, "J-001");
    assert_eq!(saved_journal.status, JournalStatus::Approved);
}

#[tokio::test]
async fn test_update_snapshot() {
    // Given: 既存のスナップショット
    let db = TestDatabase::new().await;
    let repo = SnapshotRepository::new(db.pool.clone());

    let mut journal = JournalAggregate::new();
    journal.journal_id = "J-002".to_string();
    journal.journal_date = NaiveDate::from_ymd_opt(2024, 1, 15).unwrap();
    journal.description = "最初の状態".to_string();
    journal.status = JournalStatus::Draft;

    repo.save_snapshot("Journal", "J-002", 5, &journal)
        .await
        .unwrap();

    // When: 同じ集約IDで新しいスナップショットを保存
    journal.description = "更新された状態".to_string();
    journal.status = JournalStatus::Approved;

    repo.save_snapshot("Journal", "J-002", 15, &journal)
        .await
        .unwrap();

    // Then: スナップショットが更新される
    let snapshot = repo
        .load_snapshot::<JournalAggregate>("Journal", "J-002")
        .await
        .unwrap()
        .unwrap();

    assert_eq!(snapshot.0, 15); // version
    assert_eq!(snapshot.1.description, "更新された状態");
    assert_eq!(snapshot.1.status, JournalStatus::Approved);
}

#[tokio::test]
async fn test_load_nonexistent_snapshot() {
    // Given: スナップショットリポジトリ
    let db = TestDatabase::new().await;
    let repo = SnapshotRepository::new(db.pool.clone());

    // When: 存在しないスナップショットを読み込む
    let snapshot = repo
        .load_snapshot::<JournalAggregate>("Journal", "J-999")
        .await
        .unwrap();

    // Then: None が返される
    assert!(snapshot.is_none());
}

#[tokio::test]
async fn test_delete_snapshot() {
    // Given: 既存のスナップショット
    let db = TestDatabase::new().await;
    let repo = SnapshotRepository::new(db.pool.clone());

    let journal = JournalAggregate::new();
    repo.save_snapshot("Journal", "J-003", 10, &journal)
        .await
        .unwrap();

    // When: スナップショットを削除
    repo.delete_snapshot("Journal", "J-003").await.unwrap();

    // Then: スナップショットが削除される
    let snapshot = repo
        .load_snapshot::<JournalAggregate>("Journal", "J-003")
        .await
        .unwrap();
    assert!(snapshot.is_none());
}

#[tokio::test]
async fn test_snapshot_with_entries() {
    // Given: 明細を持つ仕訳集約
    let db = TestDatabase::new().await;
    let repo = SnapshotRepository::new(db.pool.clone());

    let events = vec![
        JournalEvent::Created(JournalCreatedData {
            journal_id: "J-004".to_string(),
            journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
            description: "複合仕訳".to_string(),
        }),
        JournalEvent::EntryAdded(EntryAddedData {
            entry_id: 1,
            account_code: "1010".to_string(),
            debit_credit: "D".to_string(),
            amount: dec!(100000),
            description: "現金".to_string(),
        }),
        JournalEvent::EntryAdded(EntryAddedData {
            entry_id: 2,
            account_code: "4100".to_string(),
            debit_credit: "C".to_string(),
            amount: dec!(100000),
            description: "売上高".to_string(),
        }),
    ];

    let journal = JournalAggregate::from_events(events);

    // When: スナップショットを保存
    repo.save_snapshot("Journal", "J-004", 3, &journal)
        .await
        .unwrap();

    // Then: 明細も正しく保存・復元される
    let snapshot = repo
        .load_snapshot::<JournalAggregate>("Journal", "J-004")
        .await
        .unwrap()
        .unwrap();

    assert_eq!(snapshot.0, 3);
    assert_eq!(snapshot.1.entries.len(), 2);
    assert!(snapshot.1.entries.contains_key(&1));
    assert!(snapshot.1.entries.contains_key(&2));
    assert_eq!(snapshot.1.entries[&1].account_code, "1010");
    assert_eq!(snapshot.1.entries[&2].account_code, "4100");
}

#[tokio::test]
async fn test_snapshot_different_aggregate_types() {
    // Given: 異なる集約タイプのスナップショット
    let db = TestDatabase::new().await;
    let repo = SnapshotRepository::new(db.pool.clone());

    let journal1 = JournalAggregate::new();
    let journal2 = JournalAggregate::new();

    // When: 同じIDで異なる集約タイプを保存
    repo.save_snapshot("Journal", "AGG-001", 5, &journal1)
        .await
        .unwrap();
    repo.save_snapshot("Account", "AGG-001", 3, &journal2)
        .await
        .unwrap();

    // Then: それぞれ独立して保存される
    let journal_snapshot = repo
        .load_snapshot::<JournalAggregate>("Journal", "AGG-001")
        .await
        .unwrap()
        .unwrap();
    let account_snapshot = repo
        .load_snapshot::<JournalAggregate>("Account", "AGG-001")
        .await
        .unwrap()
        .unwrap();

    assert_eq!(journal_snapshot.0, 5);
    assert_eq!(account_snapshot.0, 3);
}
