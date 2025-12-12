use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use sqlx::PgPool;
use uuid::Uuid;

/// イベントストアのエントリ
#[derive(Debug, Clone, Serialize, Deserialize, sqlx::FromRow)]
pub struct StoredEvent {
    pub sequence_number: i64,
    pub aggregate_type: String,
    pub aggregate_id: String,
    pub event_type: String,
    pub event_version: i32,
    pub event_data: Value,
    pub metadata: Option<Value>,
    pub occurred_at: DateTime<Utc>,
    pub user_id: Option<String>,
    pub correlation_id: Option<Uuid>,
    pub causation_id: Option<Uuid>,
}

/// イベントストアエラー
#[derive(Debug, thiserror::Error)]
pub enum EventStoreError {
    #[error("Database error: {0}")]
    Database(#[from] sqlx::Error),

    #[error("Serialization error: {0}")]
    Serialization(#[from] serde_json::Error),

    #[error("Event migration error: {0}")]
    Migration(#[from] EventMigrationError),
}

/// イベントバージョン管理
pub trait EventVersioning {
    fn version(&self) -> i32;
    fn migrate_from(&self, old_version: i32, data: Value) -> Result<Value, EventMigrationError>;
}

#[derive(Debug, thiserror::Error)]
pub enum EventMigrationError {
    #[error("Unsupported migration from version {from} to {to}")]
    UnsupportedMigration { from: i32, to: i32 },

    #[error("Invalid event data: {0}")]
    InvalidData(String),
}

/// イベントストアリポジトリ
pub struct EventStoreRepository {
    pool: PgPool,
}

impl EventStoreRepository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    /// イベントを追記
    pub async fn append_event<T: Serialize>(
        &self,
        aggregate_type: &str,
        aggregate_id: &str,
        event_type: &str,
        event_data: &T,
        user_id: Option<&str>,
        correlation_id: Option<Uuid>,
    ) -> Result<i64, EventStoreError> {
        let event_data_json = serde_json::to_value(event_data)?;

        let rec = sqlx::query!(
            r#"
            INSERT INTO event_store (
                aggregate_type, aggregate_id, event_type,
                event_data, user_id, correlation_id
            )
            VALUES ($1, $2, $3, $4, $5, $6)
            RETURNING sequence_number
            "#,
            aggregate_type,
            aggregate_id,
            event_type,
            event_data_json,
            user_id,
            correlation_id
        )
        .fetch_one(&self.pool)
        .await?;

        Ok(rec.sequence_number)
    }

    /// 集約のすべてのイベントを読み込む
    pub async fn load_events(
        &self,
        aggregate_type: &str,
        aggregate_id: &str,
    ) -> Result<Vec<StoredEvent>, EventStoreError> {
        let events = sqlx::query_as!(
            StoredEvent,
            r#"
            SELECT
                sequence_number,
                aggregate_type,
                aggregate_id,
                event_type,
                event_version,
                event_data,
                metadata,
                occurred_at,
                user_id,
                correlation_id,
                causation_id
            FROM event_store
            WHERE aggregate_type = $1 AND aggregate_id = $2
            ORDER BY sequence_number ASC
            "#,
            aggregate_type,
            aggregate_id
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(events)
    }

    /// 特定のシーケンス番号以降のイベントを読み込む
    pub async fn load_events_after(
        &self,
        aggregate_type: &str,
        aggregate_id: &str,
        after_sequence: i64,
    ) -> Result<Vec<StoredEvent>, EventStoreError> {
        let events = sqlx::query_as!(
            StoredEvent,
            r#"
            SELECT
                sequence_number,
                aggregate_type,
                aggregate_id,
                event_type,
                event_version,
                event_data,
                metadata,
                occurred_at,
                user_id,
                correlation_id,
                causation_id
            FROM event_store
            WHERE aggregate_type = $1
              AND aggregate_id = $2
              AND sequence_number > $3
            ORDER BY sequence_number ASC
            "#,
            aggregate_type,
            aggregate_id,
            after_sequence
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(events)
    }

    /// イベントタイプでフィルタリング
    pub async fn load_events_by_type(
        &self,
        event_type: &str,
    ) -> Result<Vec<StoredEvent>, EventStoreError> {
        let events = sqlx::query_as!(
            StoredEvent,
            r#"
            SELECT
                sequence_number,
                aggregate_type,
                aggregate_id,
                event_type,
                event_version,
                event_data,
                metadata,
                occurred_at,
                user_id,
                correlation_id,
                causation_id
            FROM event_store
            WHERE event_type = $1
            ORDER BY sequence_number ASC
            "#,
            event_type
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(events)
    }
}
