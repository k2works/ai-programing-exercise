use async_trait::async_trait;
use crate::domain::audit::audit_log::AuditLog;

#[async_trait]
pub trait AuditLogRepository: Send + Sync {
    /// 監査ログを保存
    async fn save(&self, audit_log: AuditLog) -> Result<AuditLog, Box<dyn std::error::Error>>;

    /// エンティティの監査ログを取得
    async fn find_by_entity(
        &self,
        entity_type: &str,
        entity_id: &str,
        limit: i64,
        offset: i64,
    ) -> Result<Vec<AuditLog>, Box<dyn std::error::Error>>;

    /// ユーザーの監査ログを取得
    async fn find_by_user(
        &self,
        user_id: &str,
        limit: i64,
        offset: i64,
    ) -> Result<Vec<AuditLog>, Box<dyn std::error::Error>>;

    /// 全監査ログを取得（ページネーション）
    async fn find_all(
        &self,
        limit: i64,
        offset: i64,
    ) -> Result<Vec<AuditLog>, Box<dyn std::error::Error>>;
}
