use chrono::{DateTime, Utc};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

/// 月次勘定科目残高エンティティ
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MonthlyAccountBalance {
    /// 決算期（会計年度）
    pub fiscal_year: i32,

    /// 月度（1〜12）
    pub month: i32,

    /// 勘定科目コード
    pub account_code: String,

    /// 補助科目コード
    pub sub_account_code: String,

    /// 部門コード
    pub department_code: String,

    /// プロジェクトコード
    pub project_code: String,

    /// 決算仕訳フラグ
    pub settlement_flag: i32,

    /// 月初残高
    pub beginning_balance: Decimal,

    /// 借方金額
    pub debit_amount: Decimal,

    /// 貸方金額
    pub credit_amount: Decimal,

    /// 月末残高
    pub ending_balance: Decimal,

    /// 作成日時
    pub created_at: DateTime<Utc>,

    /// 更新日時
    pub updated_at: DateTime<Utc>,
}

impl MonthlyAccountBalance {
    /// 月末残高を計算（月初残高 + 借方金額 - 貸方金額）
    pub fn calculate_ending_balance(&self) -> Decimal {
        self.beginning_balance + self.debit_amount - self.credit_amount
    }

    /// 当月の純増減額（借方金額 - 貸方金額）
    pub fn get_net_change(&self) -> Decimal {
        self.debit_amount - self.credit_amount
    }

    /// 複合主キーを抽出
    pub fn key(&self) -> MonthlyAccountBalanceKey {
        MonthlyAccountBalanceKey {
            fiscal_year: self.fiscal_year,
            month: self.month,
            account_code: self.account_code.clone(),
            sub_account_code: self.sub_account_code.clone(),
            department_code: self.department_code.clone(),
            project_code: self.project_code.clone(),
            settlement_flag: self.settlement_flag,
        }
    }
}

/// 月次勘定科目残高の複合主キー
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MonthlyAccountBalanceKey {
    pub fiscal_year: i32,
    pub month: i32,
    pub account_code: String,
    pub sub_account_code: String,
    pub department_code: String,
    pub project_code: String,
    pub settlement_flag: i32,
}
