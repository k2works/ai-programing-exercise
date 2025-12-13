use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::HashMap;
use utoipa::ToSchema;

use crate::domain::audit::audit_log::AuditLog;

#[derive(Debug, Serialize, Deserialize, ToSchema)]
pub struct AuditLogResponse {
    pub id: i64,
    pub entity_type: String,
    pub entity_id: String,
    pub action: String,
    pub user_id: String,
    pub user_name: String,
    pub timestamp: DateTime<Utc>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub old_values: Option<HashMap<String, JsonValue>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub new_values: Option<HashMap<String, JsonValue>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub changes: Option<HashMap<String, JsonValue>>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub reason: Option<String>,
}

impl From<AuditLog> for AuditLogResponse {
    fn from(log: AuditLog) -> Self {
        Self {
            id: log.id.unwrap_or(0),
            entity_type: log.entity_type,
            entity_id: log.entity_id,
            action: log.action.to_code().to_string(),
            user_id: log.user_id,
            user_name: log.user_name,
            timestamp: log.timestamp,
            old_values: log.old_values,
            new_values: log.new_values,
            changes: log.changes,
            reason: log.reason,
        }
    }
}
