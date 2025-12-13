mod common;

use serde_json::json;
use uuid::Uuid;

use accounting_system::infrastructure::event_sourcing::event_store::EventStoreRepository;
use common::TestDatabase;

#[tokio::test]
async fn test_append_event_to_store() {
    // Given: イベントストアリポジトリ
    let db = TestDatabase::new().await;
    let repo = EventStoreRepository::new(db.pool.clone());

    // When: イベントを追加
    let aggregate_id = "journal-001";
    let event_data = json!({
        "journal_id": 1,
        "journal_date": "2024-01-15",
        "description": "売掛金の回収"
    });

    let sequence_number = repo
        .append_event(
            "Journal",
            aggregate_id,
            "JournalCreated",
            &event_data,
            Some("user@example.com"),
            Some(Uuid::new_v4()),
        )
        .await
        .unwrap();

    // Then: イベントが保存される
    assert!(sequence_number > 0);
}

#[tokio::test]
async fn test_load_events_by_aggregate() {
    // Given: イベントが保存されている
    let db = TestDatabase::new().await;
    let repo = EventStoreRepository::new(db.pool.clone());
    let aggregate_id = "journal-002";

    // 複数のイベントを追加
    for i in 1..=3 {
        repo.append_event(
            "Journal",
            aggregate_id,
            &format!("Event{}", i),
            &json!({"data": i}),
            None,
            None,
        )
        .await
        .unwrap();
    }

    // When: 集約IDでイベントを読み込む
    let events = repo.load_events("Journal", aggregate_id).await.unwrap();

    // Then: すべてのイベントが時系列順に取得される
    assert_eq!(events.len(), 3);
    assert_eq!(events[0].event_type, "Event1");
    assert_eq!(events[1].event_type, "Event2");
    assert_eq!(events[2].event_type, "Event3");
}

#[tokio::test]
async fn test_load_events_after_sequence() {
    // Given: 複数のイベントが保存されている
    let db = TestDatabase::new().await;
    let repo = EventStoreRepository::new(db.pool.clone());
    let aggregate_id = "journal-003";

    let mut sequence_numbers = Vec::new();
    for i in 1..=5 {
        let seq = repo
            .append_event(
                "Journal",
                aggregate_id,
                &format!("Event{}", i),
                &json!({"data": i}),
                None,
                None,
            )
            .await
            .unwrap();
        sequence_numbers.push(seq);
    }

    // When: 3番目のイベント以降を読み込む
    let events = repo
        .load_events_after("Journal", aggregate_id, sequence_numbers[2])
        .await
        .unwrap();

    // Then: 4番目と5番目のイベントのみ取得される
    assert_eq!(events.len(), 2);
    assert_eq!(events[0].event_type, "Event4");
    assert_eq!(events[1].event_type, "Event5");
}

#[tokio::test]
async fn test_load_events_by_type() {
    // Given: 複数のタイプのイベントが保存されている
    let db = TestDatabase::new().await;
    let repo = EventStoreRepository::new(db.pool.clone());

    // 異なるタイプのイベントを追加
    repo.append_event(
        "Journal",
        "journal-004",
        "JournalCreated",
        &json!({"data": 1}),
        None,
        None,
    )
    .await
    .unwrap();

    repo.append_event(
        "Journal",
        "journal-005",
        "JournalApproved",
        &json!({"data": 2}),
        None,
        None,
    )
    .await
    .unwrap();

    repo.append_event(
        "Journal",
        "journal-006",
        "JournalCreated",
        &json!({"data": 3}),
        None,
        None,
    )
    .await
    .unwrap();

    // When: JournalCreated タイプでフィルタリング
    let events = repo.load_events_by_type("JournalCreated").await.unwrap();

    // Then: JournalCreated イベントのみ取得される
    assert!(events.len() >= 2); // 他のテストで追加されたイベントも含まれる可能性がある
    assert!(events.iter().all(|e| e.event_type == "JournalCreated"));
}

#[tokio::test]
async fn test_event_metadata() {
    // Given: メタデータ付きイベントを保存
    let db = TestDatabase::new().await;
    let repo = EventStoreRepository::new(db.pool.clone());
    let aggregate_id = "journal-007";
    let correlation_id = Uuid::new_v4();

    repo.append_event(
        "Journal",
        aggregate_id,
        "JournalCreated",
        &json!({"amount": 10000}),
        Some("test-user"),
        Some(correlation_id),
    )
    .await
    .unwrap();

    // When: イベントを読み込む
    let events = repo.load_events("Journal", aggregate_id).await.unwrap();

    // Then: メタデータが正しく保存されている
    assert_eq!(events.len(), 1);
    assert_eq!(events[0].user_id, Some("test-user".to_string()));
    assert_eq!(events[0].correlation_id, Some(correlation_id));
    assert!(events[0].occurred_at.timestamp() > 0);
}
