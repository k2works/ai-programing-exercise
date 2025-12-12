pub mod account_events;
pub mod journal_events;

use async_trait::async_trait;
use std::fmt::Debug;

/// ドメインイベントのベーストレイト
pub trait DomainEvent: Debug + Send + Sync {
    /// イベント名を返す
    fn event_name(&self) -> &'static str;

    /// エンティティタイプを返す
    fn entity_type(&self) -> &'static str;

    /// エンティティ ID を返す
    fn entity_id(&self) -> String;
}

/// イベントハンドラーのトレイト
#[async_trait]
pub trait EventHandler<E: DomainEvent>: Send + Sync {
    /// イベントを処理
    async fn handle(&self, event: E) -> Result<(), Box<dyn std::error::Error>>;
}
