use async_trait::async_trait;

use crate::domain::journal::Journal;

/// 仕訳ユースケースのトレイト（Input Port）
#[async_trait]
pub trait JournalUseCase: Send + Sync {
    /// 仕訳を作成
    async fn create_journal(&self, journal: Journal)
        -> Result<Journal, Box<dyn std::error::Error>>;

    /// 仕訳を取得
    async fn get_journal(
        &self,
        journal_no: &str,
    ) -> Result<Option<Journal>, Box<dyn std::error::Error>>;

    /// 全仕訳を取得
    async fn get_all_journals(&self) -> Result<Vec<Journal>, Box<dyn std::error::Error>>;
}
