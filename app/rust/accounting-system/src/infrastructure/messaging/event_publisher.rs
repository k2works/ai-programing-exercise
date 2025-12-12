use async_trait::async_trait;
use lapin::{
    options::*, types::FieldTable, BasicProperties, Channel, Connection, ConnectionProperties,
};
use serde::Serialize;
use std::sync::Arc;

/// イベント発行用トレイト
#[async_trait]
pub trait EventPublisher: Send + Sync {
    async fn publish<T: Serialize + Send + Sync>(
        &self,
        event: &T,
        routing_key: &str,
    ) -> Result<(), EventPublisherError>;
}

/// RabbitMQ イベント発行実装
pub struct RabbitMQEventPublisher {
    channel: Arc<Channel>,
    exchange_name: String,
}

impl RabbitMQEventPublisher {
    pub async fn new(
        rabbitmq_url: &str,
        exchange_name: String,
    ) -> Result<Self, EventPublisherError> {
        let conn = Connection::connect(rabbitmq_url, ConnectionProperties::default()).await?;
        let channel = conn.create_channel().await?;

        // Exchange を宣言（Type: Topic）
        channel
            .exchange_declare(
                &exchange_name,
                lapin::ExchangeKind::Topic,
                ExchangeDeclareOptions {
                    durable: true,
                    ..Default::default()
                },
                FieldTable::default(),
            )
            .await?;

        Ok(Self {
            channel: Arc::new(channel),
            exchange_name,
        })
    }
}

#[async_trait]
impl EventPublisher for RabbitMQEventPublisher {
    async fn publish<T: Serialize + Send + Sync>(
        &self,
        event: &T,
        routing_key: &str,
    ) -> Result<(), EventPublisherError> {
        let payload = serde_json::to_vec(event)?;

        let properties = BasicProperties::default()
            .with_content_type("application/json".into())
            .with_delivery_mode(2) // Persistent
            .with_timestamp(chrono::Utc::now().timestamp() as u64);

        self.channel
            .basic_publish(
                &self.exchange_name,
                routing_key,
                BasicPublishOptions::default(),
                &payload,
                properties,
            )
            .await?
            .await?;

        tracing::info!("Published event to routing_key: {}", routing_key);
        Ok(())
    }
}

/// イベント発行エラー
#[derive(Debug, thiserror::Error)]
pub enum EventPublisherError {
    #[error("RabbitMQ error: {0}")]
    RabbitMQ(#[from] lapin::Error),

    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),
}
