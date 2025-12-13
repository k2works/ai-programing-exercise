use async_trait::async_trait;
use serde_json::json;
use std::collections::HashMap;
use std::sync::Arc;

use crate::application::ports::input::account_usecase::AccountUseCase;
use crate::application::ports::output::account_repository::AccountRepository;
use crate::domain::account::Account;
use crate::domain::events::account_events::{AccountCreatedEvent, AccountUpdatedEvent};
use crate::domain::events::EventHandler;

/// 勘定科目サービス（Application Service）
pub struct AccountService<H>
where
    H: EventHandler<AccountCreatedEvent> + EventHandler<AccountUpdatedEvent>,
{
    repository: Arc<dyn AccountRepository>,
    event_handler: Arc<H>,
}

impl<H> AccountService<H>
where
    H: EventHandler<AccountCreatedEvent> + EventHandler<AccountUpdatedEvent>,
{
    pub fn new(repository: Arc<dyn AccountRepository>, event_handler: Arc<H>) -> Self {
        Self {
            repository,
            event_handler,
        }
    }
}

/// Account を JSON Map に変換するヘルパー関数
fn account_to_json(account: &Account) -> HashMap<String, serde_json::Value> {
    let mut map = HashMap::new();
    if let Some(id) = account.account_id {
        map.insert("account_id".to_string(), json!(id));
    }
    map.insert("account_code".to_string(), json!(account.account_code));
    map.insert("account_name".to_string(), json!(account.account_name));
    if let Some(ref kana) = account.account_name_kana {
        map.insert("account_name_kana".to_string(), json!(kana));
    }
    map.insert("account_type".to_string(), json!(account.account_type));
    map.insert(
        "is_summary_account".to_string(),
        json!(account.is_summary_account),
    );
    if let Some(ref bspl) = account.bspl_type {
        map.insert("bspl_type".to_string(), json!(bspl));
    }
    if let Some(ref tx_elem) = account.transaction_element_type {
        map.insert("transaction_element_type".to_string(), json!(tx_elem));
    }
    if let Some(ref expense) = account.expense_type {
        map.insert("expense_type".to_string(), json!(expense));
    }
    map.insert("display_order".to_string(), json!(account.display_order));
    map.insert(
        "is_aggregation_target".to_string(),
        json!(account.is_aggregation_target),
    );
    if let Some(ref tax) = account.tax_code {
        map.insert("tax_code".to_string(), json!(tax));
    }
    map.insert("balance".to_string(), json!(account.balance.to_string()));
    map
}

#[async_trait]
impl<H> AccountUseCase for AccountService<H>
where
    H: EventHandler<AccountCreatedEvent> + EventHandler<AccountUpdatedEvent>,
{
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
        user_id: String,
        user_name: String,
        ip_address: Option<String>,
    ) -> Result<Account, Box<dyn std::error::Error>> {
        // ビジネスロジック検証
        if account.account_code.is_empty() {
            return Err("Account code cannot be empty".into());
        }

        // リポジトリに保存
        let created_account = self.repository.create(account.clone()).await?;

        // イベント発行
        let event = AccountCreatedEvent {
            account_code: created_account.account_code.clone(),
            new_values: account_to_json(&created_account),
            user_id,
            user_name,
            ip_address,
        };

        self.event_handler.handle(event).await?;

        Ok(created_account)
    }

    async fn update_account(
        &self,
        account: Account,
        user_id: String,
        user_name: String,
        ip_address: Option<String>,
    ) -> Result<Account, Box<dyn std::error::Error>> {
        // ビジネスロジック検証
        if account.account_code.is_empty() {
            return Err("Account code cannot be empty".into());
        }

        // 更新前の状態を取得
        let old_account = self
            .repository
            .find_by_code(&account.account_code)
            .await?
            .ok_or("Account not found")?;

        // リポジトリで更新
        let updated_account = self.repository.update(account.clone()).await?;

        // イベント発行
        let event = AccountUpdatedEvent {
            account_code: updated_account.account_code.clone(),
            old_values: account_to_json(&old_account),
            new_values: account_to_json(&updated_account),
            user_id,
            user_name,
            ip_address,
        };

        self.event_handler.handle(event).await?;

        Ok(updated_account)
    }
}
