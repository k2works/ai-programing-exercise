use serde_json::Value as JsonValue;
use std::collections::HashMap;

use super::DomainEvent;

/// 勘定科目更新イベント
#[derive(Debug, Clone)]
pub struct AccountUpdatedEvent {
    pub account_code: String,
    pub old_values: HashMap<String, JsonValue>,
    pub new_values: HashMap<String, JsonValue>,
    pub user_id: String,
    pub user_name: String,
    pub ip_address: Option<String>,
}

impl DomainEvent for AccountUpdatedEvent {
    fn event_name(&self) -> &'static str {
        "AccountUpdated"
    }

    fn entity_type(&self) -> &'static str {
        "Account"
    }

    fn entity_id(&self) -> String {
        self.account_code.clone()
    }
}

/// 勘定科目作成イベント
#[derive(Debug, Clone)]
pub struct AccountCreatedEvent {
    pub account_code: String,
    pub new_values: HashMap<String, JsonValue>,
    pub user_id: String,
    pub user_name: String,
    pub ip_address: Option<String>,
}

impl DomainEvent for AccountCreatedEvent {
    fn event_name(&self) -> &'static str {
        "AccountCreated"
    }

    fn entity_type(&self) -> &'static str {
        "Account"
    }

    fn entity_id(&self) -> String {
        self.account_code.clone()
    }
}

/// 勘定科目削除イベント
#[derive(Debug, Clone)]
pub struct AccountDeletedEvent {
    pub account_code: String,
    pub old_values: HashMap<String, JsonValue>,
    pub user_id: String,
    pub user_name: String,
    pub reason: Option<String>,
    pub ip_address: Option<String>,
}

impl DomainEvent for AccountDeletedEvent {
    fn event_name(&self) -> &'static str {
        "AccountDeleted"
    }

    fn entity_type(&self) -> &'static str {
        "Account"
    }

    fn entity_id(&self) -> String {
        self.account_code.clone()
    }
}
