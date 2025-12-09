use chrono::{DateTime, NaiveDate, Utc};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

/// 日次勘定科目残高エンティティ
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DailyAccountBalance {
    /// 起票日
    pub entry_date: NaiveDate,

    /// 勘定科目コード
    pub account_code: String,

    /// 補助科目コード
    pub sub_account_code: String,

    /// 部門コード
    pub department_code: String,

    /// プロジェクトコード
    pub project_code: String,

    /// 決算仕訳フラグ（0=通常、1=決算）
    pub settlement_flag: i32,

    /// 借方金額
    pub debit_amount: Decimal,

    /// 貸方金額
    pub credit_amount: Decimal,

    /// 作成日時
    pub created_at: DateTime<Utc>,

    /// 更新日時
    pub updated_at: DateTime<Utc>,
}

impl DailyAccountBalance {
    /// 残高を計算（借方金額 - 貸方金額）
    pub fn get_balance(&self) -> Decimal {
        self.debit_amount - self.credit_amount
    }

    /// 借方と貸方の合計金額
    pub fn get_total_amount(&self) -> Decimal {
        self.debit_amount + self.credit_amount
    }

    /// 複合主キーを抽出
    pub fn key(&self) -> DailyAccountBalanceKey {
        DailyAccountBalanceKey {
            entry_date: self.entry_date,
            account_code: self.account_code.clone(),
            sub_account_code: self.sub_account_code.clone(),
            department_code: self.department_code.clone(),
            project_code: self.project_code.clone(),
            settlement_flag: self.settlement_flag,
        }
    }
}

/// 日次勘定科目残高の複合主キー
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct DailyAccountBalanceKey {
    pub entry_date: NaiveDate,
    pub account_code: String,
    pub sub_account_code: String,
    pub department_code: String,
    pub project_code: String,
    pub settlement_flag: i32,
}
