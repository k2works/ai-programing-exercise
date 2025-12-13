use crate::domain::account::Account;
use async_trait::async_trait;

/// 勘定科目リポジトリのトレイト（Output Port）
#[async_trait]
pub trait AccountRepository: Send + Sync {
    /// 全勘定科目を取得
    async fn find_all(&self) -> Result<Vec<Account>, Box<dyn std::error::Error>>;

    /// 勘定科目コードで検索
    async fn find_by_code(&self, code: &str)
        -> Result<Option<Account>, Box<dyn std::error::Error>>;

    /// 勘定科目を作成
    async fn create(&self, account: Account) -> Result<Account, Box<dyn std::error::Error>>;

    /// 勘定科目を更新
    async fn update(&self, account: Account) -> Result<Account, Box<dyn std::error::Error>>;
}
