use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;
use std::collections::HashMap;

use super::audit_action::AuditAction;

/// 監査ログドメインモデル（不変）
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AuditLog {
    #[serde(skip_serializing_if = "Option::is_none")]
    pub id: Option<i64>,
    pub entity_type: String,
    pub entity_id: String,
    pub action: AuditAction,
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
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ip_address: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub user_agent: Option<String>,
}

impl AuditLog {
    /// CREATE 操作用のコンストラクタ
    pub fn new_create(
        entity_type: String,
        entity_id: String,
        user_id: String,
        user_name: String,
        new_values: HashMap<String, JsonValue>,
        ip_address: Option<String>,
    ) -> Self {
        Self {
            id: None,
            entity_type,
            entity_id,
            action: AuditAction::Create,
            user_id,
            user_name,
            timestamp: Utc::now(),
            old_values: None,
            new_values: Some(new_values),
            changes: None,
            reason: None,
            ip_address,
            user_agent: None,
        }
    }

    /// UPDATE 操作用のコンストラクタ
    pub fn new_update(
        entity_type: String,
        entity_id: String,
        user_id: String,
        user_name: String,
        old_values: HashMap<String, JsonValue>,
        new_values: HashMap<String, JsonValue>,
        ip_address: Option<String>,
    ) -> Self {
        // 変更された項目のみを changes に記録
        let changes: HashMap<String, JsonValue> = new_values
            .iter()
            .filter(|(k, v)| {
                old_values
                    .get(*k)
                    .map(|old_v| old_v != *v)
                    .unwrap_or(true)
            })
            .map(|(k, v)| (k.clone(), v.clone()))
            .collect();

        Self {
            id: None,
            entity_type,
            entity_id,
            action: AuditAction::Update,
            user_id,
            user_name,
            timestamp: Utc::now(),
            old_values: Some(old_values),
            new_values: Some(new_values),
            changes: Some(changes),
            reason: None,
            ip_address,
            user_agent: None,
        }
    }

    /// DELETE 操作用のコンストラクタ
    pub fn new_delete(
        entity_type: String,
        entity_id: String,
        user_id: String,
        user_name: String,
        old_values: HashMap<String, JsonValue>,
        reason: Option<String>,
        ip_address: Option<String>,
    ) -> Self {
        Self {
            id: None,
            entity_type,
            entity_id,
            action: AuditAction::Delete,
            user_id,
            user_name,
            timestamp: Utc::now(),
            old_values: Some(old_values),
            new_values: None,
            changes: None,
            reason,
            ip_address,
            user_agent: None,
        }
    }
}
