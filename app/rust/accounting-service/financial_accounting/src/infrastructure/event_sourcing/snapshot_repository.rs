use serde::{Deserialize, Serialize};
use sqlx::PgPool;

/// スナップショットリポジトリ
pub struct SnapshotRepository {
    pool: PgPool,
}

impl SnapshotRepository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    /// スナップショットを保存
    pub async fn save_snapshot<T: Serialize>(
        &self,
        aggregate_type: &str,
        aggregate_id: &str,
        version: i64,
        state: &T,
    ) -> Result<(), SnapshotError> {
        let snapshot_data = serde_json::to_value(state)?;

        sqlx::query!(
            r#"
            INSERT INTO snapshots (aggregate_type, aggregate_id, version, snapshot_data)
            VALUES ($1, $2, $3, $4)
            ON CONFLICT (aggregate_type, aggregate_id)
            DO UPDATE SET
                version = EXCLUDED.version,
                snapshot_data = EXCLUDED.snapshot_data,
                created_at = NOW()
            "#,
            aggregate_type,
            aggregate_id,
            version,
            snapshot_data
        )
        .execute(&self.pool)
        .await?;

        Ok(())
    }

    /// スナップショットを読み込み
    pub async fn load_snapshot<T>(
        &self,
        aggregate_type: &str,
        aggregate_id: &str,
    ) -> Result<Option<(i64, T)>, SnapshotError>
    where
        T: for<'de> Deserialize<'de>,
    {
        let result = sqlx::query!(
            r#"
            SELECT version, snapshot_data
            FROM snapshots
            WHERE aggregate_type = $1 AND aggregate_id = $2
            "#,
            aggregate_type,
            aggregate_id
        )
        .fetch_optional(&self.pool)
        .await?;

        match result {
            Some(row) => {
                let state: T = serde_json::from_value(row.snapshot_data)?;
                Ok(Some((row.version, state)))
            }
            None => Ok(None),
        }
    }

    /// スナップショットを削除
    pub async fn delete_snapshot(
        &self,
        aggregate_type: &str,
        aggregate_id: &str,
    ) -> Result<(), SnapshotError> {
        sqlx::query!(
            "DELETE FROM snapshots WHERE aggregate_type = $1 AND aggregate_id = $2",
            aggregate_type,
            aggregate_id
        )
        .execute(&self.pool)
        .await?;

        Ok(())
    }
}

/// スナップショットエラー
#[derive(Debug, thiserror::Error)]
pub enum SnapshotError {
    #[error("Database error: {0}")]
    DatabaseError(#[from] sqlx::Error),

    #[error("Serialization error: {0}")]
    SerializationError(#[from] serde_json::Error),
}
