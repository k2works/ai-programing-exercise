use crate::domain::account::Account;
use chrono::{DateTime, Utc};
use rust_decimal::Decimal;
use serde::Serialize;
use utoipa::ToSchema;

/// 勘定科目レスポンス
#[derive(Debug, Serialize, ToSchema)]
pub struct AccountResponse {
    pub account_id: Option<i32>,
    pub account_code: String,
    pub account_name: String,
    pub account_name_kana: Option<String>,
    pub account_type: String,
    pub is_summary_account: bool,
    pub bspl_type: Option<String>,
    pub transaction_element_type: Option<String>,
    pub expense_type: Option<String>,
    pub display_order: i32,
    pub is_aggregation_target: bool,
    pub tax_code: Option<String>,
    pub balance: Decimal,
    pub created_at: Option<DateTime<Utc>>,
    pub updated_at: Option<DateTime<Utc>>,
}

impl From<Account> for AccountResponse {
    fn from(account: Account) -> Self {
        AccountResponse {
            account_id: account.account_id,
            account_code: account.account_code,
            account_name: account.account_name,
            account_name_kana: account.account_name_kana,
            account_type: account.account_type,
            is_summary_account: account.is_summary_account,
            bspl_type: account.bspl_type,
            transaction_element_type: account.transaction_element_type,
            expense_type: account.expense_type,
            display_order: account.display_order,
            is_aggregation_target: account.is_aggregation_target,
            tax_code: account.tax_code,
            balance: account.balance,
            created_at: account.created_at,
            updated_at: account.updated_at,
        }
    }
}
