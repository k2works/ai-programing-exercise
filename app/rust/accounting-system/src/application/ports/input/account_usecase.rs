use crate::domain::account::Account;
use async_trait::async_trait;

/// 勘定科目ユースケースのトレイト（Input Port）
#[async_trait]
pub trait AccountUseCase: Send + Sync {
    /// 全勘定科目を取得
    async fn get_all_accounts(&self) -> Result<Vec<Account>, Box<dyn std::error::Error>>;

    /// 勘定科目コードで検索
    async fn get_account_by_code(
        &self,
        code: &str,
    ) -> Result<Option<Account>, Box<dyn std::error::Error>>;

    /// 勘定科目を作成
    async fn create_account(&self, account: Account)
        -> Result<Account, Box<dyn std::error::Error>>;
}
