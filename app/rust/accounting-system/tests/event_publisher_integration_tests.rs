mod common;

use accounting_system::infrastructure::messaging::event_publisher::*;
use common::TestRabbitMQ;
use serde::{Deserialize, Serialize};

/// RabbitMQ パブリッシャーの作成をリトライする
async fn retry_create_publisher(url: &str, exchange_name: String) -> RabbitMQEventPublisher {
    let max_retries = 15;
    let retry_delay = std::time::Duration::from_secs(3);

    for attempt in 1..=max_retries {
        match RabbitMQEventPublisher::new(url, exchange_name.clone()).await {
            Ok(publisher) => return publisher,
            Err(e) => {
                if attempt == max_retries {
                    panic!(
                        "Failed to create publisher after {} attempts: {:?}",
                        max_retries, e
                    );
                }
                eprintln!(
                    "Attempt {}/{} failed: {:?}, retrying...",
                    attempt, max_retries, e
                );
                tokio::time::sleep(retry_delay).await;
            }
        }
    }
    unreachable!()
}

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TestEvent {
    id: u32,
    message: String,
}

#[tokio::test]
async fn test_publish_event_with_testcontainers() {
    // Given: testcontainers で RabbitMQ を起動
    let rabbitmq = TestRabbitMQ::new();

    // RabbitMQ が起動するまでリトライ
    let publisher = retry_create_publisher(&rabbitmq.url, "test.exchange".to_string()).await;

    // When: イベントを発行
    let event = TestEvent {
        id: 1,
        message: "Test event with testcontainers".to_string(),
    };

    let result = publisher.publish(&event, "test.routing.key").await;

    // Then: 成功する
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_publish_multiple_events_with_testcontainers() {
    // Given: testcontainers で RabbitMQ を起動
    let rabbitmq = TestRabbitMQ::new();

    // RabbitMQ が起動するまでリトライ
    let publisher = retry_create_publisher(&rabbitmq.url, "test.exchange".to_string()).await;

    // When: 複数のイベントを発行
    for i in 1..=10 {
        let event = TestEvent {
            id: i,
            message: format!("Event number {} with testcontainers", i),
        };

        let result = publisher
            .publish(&event, &format!("test.event.{}", i))
            .await;

        assert!(result.is_ok(), "Failed to publish event {}", i);
    }

    // Then: すべて成功する
}

#[tokio::test]
async fn test_publish_different_routing_keys() {
    // Given: testcontainers で RabbitMQ を起動
    let rabbitmq = TestRabbitMQ::new();

    // RabbitMQ が起動するまでリトライ
    let publisher = retry_create_publisher(&rabbitmq.url, "accounting.events".to_string()).await;

    // When: 異なるルーティングキーでイベントを発行
    let routing_keys = vec![
        "journal.created",
        "journal.approved",
        "journal.canceled",
        "account.created",
        "account.updated",
    ];

    for (i, routing_key) in routing_keys.iter().enumerate() {
        let event = TestEvent {
            id: i as u32,
            message: format!("Event for {}", routing_key),
        };

        let result = publisher.publish(&event, routing_key).await;
        assert!(result.is_ok(), "Failed to publish to {}", routing_key);
    }

    // Then: すべて成功する
}

#[tokio::test]
async fn test_publisher_with_json_serialization() {
    // Given: testcontainers で RabbitMQ を起動
    let rabbitmq = TestRabbitMQ::new();

    // RabbitMQ が起動するまでリトライ
    let publisher = retry_create_publisher(&rabbitmq.url, "test.exchange".to_string()).await;

    // When: 複雑な構造のイベントを発行
    #[derive(Debug, Serialize, Deserialize)]
    struct ComplexEvent {
        id: u32,
        name: String,
        tags: Vec<String>,
        metadata: std::collections::HashMap<String, String>,
    }

    let mut metadata = std::collections::HashMap::new();
    metadata.insert("user".to_string(), "test-user".to_string());
    metadata.insert("source".to_string(), "integration-test".to_string());

    let event = ComplexEvent {
        id: 42,
        name: "Complex event".to_string(),
        tags: vec!["test".to_string(), "integration".to_string()],
        metadata,
    };

    let result = publisher.publish(&event, "test.complex").await;

    // Then: 成功する
    assert!(result.is_ok());
}
