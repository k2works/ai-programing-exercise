use async_trait::async_trait;
use std::sync::Arc;

use crate::application::ports::output::audit_log_repository::AuditLogRepository;
use crate::domain::audit::audit_log::AuditLog;
use crate::domain::events::account_events::{
    AccountCreatedEvent, AccountDeletedEvent, AccountUpdatedEvent,
};
use crate::domain::events::{DomainEvent, EventHandler};

/// 監査ログイベントハンドラー
pub struct AuditLogEventHandler {
    repository: Arc<dyn AuditLogRepository>,
}

impl AuditLogEventHandler {
    pub fn new(repository: Arc<dyn AuditLogRepository>) -> Self {
        Self { repository }
    }
}

#[async_trait]
impl EventHandler<AccountUpdatedEvent> for AuditLogEventHandler {
    async fn handle(&self, event: AccountUpdatedEvent) -> Result<(), Box<dyn std::error::Error>> {
        let audit_log = AuditLog::new_update(
            event.entity_type().to_string(),
            event.entity_id(),
            event.user_id,
            event.user_name,
            event.old_values,
            event.new_values,
            event.ip_address,
        );

        self.repository.save(audit_log).await?;
        Ok(())
    }
}

#[async_trait]
impl EventHandler<AccountCreatedEvent> for AuditLogEventHandler {
    async fn handle(&self, event: AccountCreatedEvent) -> Result<(), Box<dyn std::error::Error>> {
        let audit_log = AuditLog::new_create(
            event.entity_type().to_string(),
            event.entity_id(),
            event.user_id,
            event.user_name,
            event.new_values,
            event.ip_address,
        );

        self.repository.save(audit_log).await?;
        Ok(())
    }
}

#[async_trait]
impl EventHandler<AccountDeletedEvent> for AuditLogEventHandler {
    async fn handle(&self, event: AccountDeletedEvent) -> Result<(), Box<dyn std::error::Error>> {
        let audit_log = AuditLog::new_delete(
            event.entity_type().to_string(),
            event.entity_id(),
            event.user_id,
            event.user_name,
            event.old_values,
            event.reason,
            event.ip_address,
        );

        self.repository.save(audit_log).await?;
        Ok(())
    }
}
