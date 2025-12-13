use accounting_system::infrastructure::messaging::event_publisher::*;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
struct TestEvent {
    id: u32,
    message: String,
}

#[tokio::test]
#[ignore] // RabbitMQ が起動している場合のみ実行
async fn test_publish_event_to_rabbitmq() {
    // Given: RabbitMQ イベントパブリッシャー
    let publisher = RabbitMQEventPublisher::new(
        "amqp://guest:guest@localhost:5672",
        "test.exchange".to_string(),
    )
    .await
    .expect("Failed to create publisher");

    // When: イベントを発行
    let event = TestEvent {
        id: 1,
        message: "Test event message".to_string(),
    };

    let result = publisher.publish(&event, "test.routing.key").await;

    // Then: 成功する
    assert!(result.is_ok());
}

#[tokio::test]
#[ignore] // RabbitMQ が起動している場合のみ実行
async fn test_publish_multiple_events() {
    // Given: RabbitMQ イベントパブリッシャー
    let publisher = RabbitMQEventPublisher::new(
        "amqp://guest:guest@localhost:5672",
        "test.exchange".to_string(),
    )
    .await
    .expect("Failed to create publisher");

    // When: 複数のイベントを発行
    for i in 1..=5 {
        let event = TestEvent {
            id: i,
            message: format!("Event number {}", i),
        };

        let result = publisher
            .publish(&event, &format!("test.event.{}", i))
            .await;

        assert!(result.is_ok());
    }

    // Then: すべて成功する
}

#[tokio::test]
async fn test_connection_error() {
    // Given: 間違った RabbitMQ URL
    let result = RabbitMQEventPublisher::new(
        "amqp://invalid:invalid@localhost:9999",
        "test.exchange".to_string(),
    )
    .await;

    // Then: エラーが返される
    assert!(result.is_err());
}
