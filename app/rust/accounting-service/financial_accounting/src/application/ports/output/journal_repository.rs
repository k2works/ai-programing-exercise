use async_trait::async_trait;

use crate::domain::journal::Journal;

/// 仕訳リポジトリのトレイト（Output Port）
#[async_trait]
pub trait JournalRepository: Send + Sync {
    /// 仕訳を作成
    async fn create(&self, journal: Journal) -> Result<Journal, Box<dyn std::error::Error>>;

    /// 仕訳伝票番号で検索
    async fn find_by_no(
        &self,
        journal_no: &str,
    ) -> Result<Option<Journal>, Box<dyn std::error::Error>>;

    /// 全仕訳を取得
    async fn find_all(&self) -> Result<Vec<Journal>, Box<dyn std::error::Error>>;
}
