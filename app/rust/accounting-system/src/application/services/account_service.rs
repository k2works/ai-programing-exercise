use async_trait::async_trait;
use std::sync::Arc;

use crate::application::ports::input::account_usecase::AccountUseCase;
use crate::application::ports::output::account_repository::AccountRepository;
use crate::domain::account::Account;

/// 勘定科目サービス（Application Service）
pub struct AccountService {
    repository: Arc<dyn AccountRepository>,
}

impl AccountService {
    pub fn new(repository: Arc<dyn AccountRepository>) -> Self {
        Self { repository }
    }
}

#[async_trait]
impl AccountUseCase for AccountService {
    async fn get_all_accounts(&self) -> Result<Vec<Account>, Box<dyn std::error::Error>> {
        self.repository.find_all().await
    }

    async fn get_account_by_code(
        &self,
        code: &str,
    ) -> Result<Option<Account>, Box<dyn std::error::Error>> {
        self.repository.find_by_code(code).await
    }

    async fn create_account(
        &self,
        account: Account,
    ) -> Result<Account, Box<dyn std::error::Error>> {
        // ビジネスロジック検証
        if account.account_code.is_empty() {
            return Err("Account code cannot be empty".into());
        }

        self.repository.create(account).await
    }
}
