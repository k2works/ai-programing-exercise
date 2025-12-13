use async_trait::async_trait;
use sqlx::PgPool;

use crate::application::ports::output::audit_log_repository::AuditLogRepository;
use crate::domain::audit::audit_action::AuditAction;
use crate::domain::audit::audit_log::AuditLog;

pub struct AuditLogRepositoryImpl {
    pool: PgPool,
}

impl AuditLogRepositoryImpl {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }
}

#[async_trait]
impl AuditLogRepository for AuditLogRepositoryImpl {
    async fn save(&self, audit_log: AuditLog) -> Result<AuditLog, Box<dyn std::error::Error>> {
        let action_code = audit_log.action.to_code();

        let rec = sqlx::query!(
            r#"
            INSERT INTO audit_log (
                entity_type, entity_id, action, user_id, user_name,
                timestamp, old_values, new_values, changes, reason, ip_address, user_agent
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
            RETURNING
                id, entity_type, entity_id, action, user_id, user_name,
                timestamp, old_values, new_values, changes, reason, ip_address, user_agent,
                created_at
            "#,
            audit_log.entity_type,
            audit_log.entity_id,
            action_code,
            audit_log.user_id,
            audit_log.user_name,
            audit_log.timestamp,
            serde_json::to_value(&audit_log.old_values)?,
            serde_json::to_value(&audit_log.new_values)?,
            serde_json::to_value(&audit_log.changes)?,
            audit_log.reason,
            audit_log.ip_address,
            audit_log.user_agent,
        )
        .fetch_one(&self.pool)
        .await?;

        Ok(AuditLog {
            id: Some(rec.id),
            entity_type: rec.entity_type,
            entity_id: rec.entity_id,
            action: AuditAction::from_code(&rec.action).unwrap(),
            user_id: rec.user_id,
            user_name: rec.user_name,
            timestamp: rec.timestamp,
            old_values: rec.old_values.and_then(|v| serde_json::from_value(v).ok()),
            new_values: rec.new_values.and_then(|v| serde_json::from_value(v).ok()),
            changes: rec.changes.and_then(|v| serde_json::from_value(v).ok()),
            reason: rec.reason,
            ip_address: rec.ip_address,
            user_agent: rec.user_agent,
        })
    }

    async fn find_by_entity(
        &self,
        entity_type: &str,
        entity_id: &str,
        limit: i64,
        offset: i64,
    ) -> Result<Vec<AuditLog>, Box<dyn std::error::Error>> {
        let logs = sqlx::query!(
            r#"
            SELECT
                id, entity_type, entity_id, action, user_id, user_name,
                timestamp, old_values, new_values, changes, reason, ip_address, user_agent,
                created_at
            FROM audit_log
            WHERE entity_type = $1 AND entity_id = $2
            ORDER BY timestamp DESC
            LIMIT $3 OFFSET $4
            "#,
            entity_type,
            entity_id,
            limit,
            offset,
        )
        .fetch_all(&self.pool)
        .await?;

        let result = logs
            .into_iter()
            .map(|rec| AuditLog {
                id: Some(rec.id),
                entity_type: rec.entity_type,
                entity_id: rec.entity_id,
                action: AuditAction::from_code(&rec.action).unwrap(),
                user_id: rec.user_id,
                user_name: rec.user_name,
                timestamp: rec.timestamp,
                old_values: rec.old_values.and_then(|v| serde_json::from_value(v).ok()),
                new_values: rec.new_values.and_then(|v| serde_json::from_value(v).ok()),
                changes: rec.changes.and_then(|v| serde_json::from_value(v).ok()),
                reason: rec.reason,
                ip_address: rec.ip_address,
                user_agent: rec.user_agent,
            })
            .collect();

        Ok(result)
    }

    async fn find_by_user(
        &self,
        user_id: &str,
        limit: i64,
        offset: i64,
    ) -> Result<Vec<AuditLog>, Box<dyn std::error::Error>> {
        let logs = sqlx::query!(
            r#"
            SELECT
                id, entity_type, entity_id, action, user_id, user_name,
                timestamp, old_values, new_values, changes, reason, ip_address, user_agent,
                created_at
            FROM audit_log
            WHERE user_id = $1
            ORDER BY timestamp DESC
            LIMIT $2 OFFSET $3
            "#,
            user_id,
            limit,
            offset,
        )
        .fetch_all(&self.pool)
        .await?;

        let result = logs
            .into_iter()
            .map(|rec| AuditLog {
                id: Some(rec.id),
                entity_type: rec.entity_type,
                entity_id: rec.entity_id,
                action: AuditAction::from_code(&rec.action).unwrap(),
                user_id: rec.user_id,
                user_name: rec.user_name,
                timestamp: rec.timestamp,
                old_values: rec.old_values.and_then(|v| serde_json::from_value(v).ok()),
                new_values: rec.new_values.and_then(|v| serde_json::from_value(v).ok()),
                changes: rec.changes.and_then(|v| serde_json::from_value(v).ok()),
                reason: rec.reason,
                ip_address: rec.ip_address,
                user_agent: rec.user_agent,
            })
            .collect();

        Ok(result)
    }

    async fn find_all(
        &self,
        limit: i64,
        offset: i64,
    ) -> Result<Vec<AuditLog>, Box<dyn std::error::Error>> {
        let logs = sqlx::query!(
            r#"
            SELECT
                id, entity_type, entity_id, action, user_id, user_name,
                timestamp, old_values, new_values, changes, reason, ip_address, user_agent,
                created_at
            FROM audit_log
            ORDER BY timestamp DESC
            LIMIT $1 OFFSET $2
            "#,
            limit,
            offset,
        )
        .fetch_all(&self.pool)
        .await?;

        let result = logs
            .into_iter()
            .map(|rec| AuditLog {
                id: Some(rec.id),
                entity_type: rec.entity_type,
                entity_id: rec.entity_id,
                action: AuditAction::from_code(&rec.action).unwrap(),
                user_id: rec.user_id,
                user_name: rec.user_name,
                timestamp: rec.timestamp,
                old_values: rec.old_values.and_then(|v| serde_json::from_value(v).ok()),
                new_values: rec.new_values.and_then(|v| serde_json::from_value(v).ok()),
                changes: rec.changes.and_then(|v| serde_json::from_value(v).ok()),
                reason: rec.reason,
                ip_address: rec.ip_address,
                user_agent: rec.user_agent,
            })
            .collect();

        Ok(result)
    }
}
