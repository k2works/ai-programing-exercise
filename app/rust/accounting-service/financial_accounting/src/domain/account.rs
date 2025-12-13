use chrono::{DateTime, Utc};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;
use utoipa::ToSchema;

/// 勘定科目エンティティ
/// データベースの日本語カラム名と英語プロパティ名をマッピング
#[derive(Debug, Clone, Serialize, Deserialize, FromRow, ToSchema)]
pub struct Account {
    #[sqlx(rename = "勘定科目ID")]
    pub account_id: Option<i32>,
    #[sqlx(rename = "勘定科目コード")]
    pub account_code: String,
    #[sqlx(rename = "勘定科目名")]
    pub account_name: String,
    #[sqlx(rename = "勘定科目カナ")]
    pub account_name_kana: Option<String>,
    #[sqlx(rename = "勘定科目種別")]
    pub account_type: String,
    #[sqlx(rename = "合計科目")]
    pub is_summary_account: bool,
    #[sqlx(rename = "BSPL区分")]
    pub bspl_type: Option<String>,
    #[sqlx(rename = "取引要素区分")]
    pub transaction_element_type: Option<String>,
    #[sqlx(rename = "費用区分")]
    pub expense_type: Option<String>,
    #[sqlx(rename = "表示順序")]
    pub display_order: i32,
    #[sqlx(rename = "集計対象")]
    pub is_aggregation_target: bool,
    #[sqlx(rename = "課税取引コード")]
    pub tax_code: Option<String>,
    #[sqlx(rename = "残高")]
    pub balance: Decimal,
    #[sqlx(rename = "作成日時")]
    pub created_at: Option<DateTime<Utc>>,
    #[sqlx(rename = "更新日時")]
    pub updated_at: Option<DateTime<Utc>>,
}

impl Account {
    /// 新規勘定科目を作成
    pub fn new(
        account_code: String,
        account_name: String,
        account_type: String,
        is_summary_account: bool,
    ) -> Self {
        Self {
            account_id: None,
            account_code,
            account_name,
            account_name_kana: None,
            account_type,
            is_summary_account,
            bspl_type: None,
            transaction_element_type: None,
            expense_type: None,
            display_order: 0,
            is_aggregation_target: true,
            tax_code: None,
            balance: Decimal::ZERO,
            created_at: None,
            updated_at: None,
        }
    }
}
