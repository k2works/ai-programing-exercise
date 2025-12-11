use crate::domain::account::Account;
use serde::Deserialize;
use utoipa::ToSchema;

/// 勘定科目作成リクエスト
#[derive(Debug, Deserialize, ToSchema)]
pub struct AccountRequest {
    pub account_code: String,
    pub account_name: String,
    pub account_name_kana: Option<String>,
    pub account_type: String,
    pub is_summary_account: bool,
    pub bspl_type: Option<String>,
    pub transaction_element_type: Option<String>,
    pub expense_type: Option<String>,
    pub display_order: Option<i32>,
    pub is_aggregation_target: Option<bool>,
    pub tax_code: Option<String>,
}

impl From<AccountRequest> for Account {
    fn from(req: AccountRequest) -> Self {
        Account {
            account_id: None,
            account_code: req.account_code,
            account_name: req.account_name,
            account_name_kana: req.account_name_kana,
            account_type: req.account_type,
            is_summary_account: req.is_summary_account,
            bspl_type: req.bspl_type,
            transaction_element_type: req.transaction_element_type,
            expense_type: req.expense_type,
            display_order: req.display_order.unwrap_or(0),
            is_aggregation_target: req.is_aggregation_target.unwrap_or(true),
            tax_code: req.tax_code,
            balance: rust_decimal::Decimal::ZERO,
            created_at: None,
            updated_at: None,
        }
    }
}
