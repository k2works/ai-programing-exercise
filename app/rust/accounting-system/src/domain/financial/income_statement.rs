use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

/// 損益計算書の項目
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IncomeStatementItem {
    /// 勘定科目コード
    pub account_code: String,

    /// 勘定科目名
    pub account_name: String,

    /// 金額
    pub amount: Decimal,

    /// 構成比率（%）
    pub percentage: Decimal,
}

/// 損益計算書
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IncomeStatement {
    /// 期間開始日
    pub start_date: NaiveDate,

    /// 期間終了日
    pub end_date: NaiveDate,

    /// 収益項目
    pub revenues: Vec<IncomeStatementItem>,

    /// 費用項目
    pub expenses: Vec<IncomeStatementItem>,

    /// 売上高
    pub total_sales: Decimal,

    /// 売上原価
    pub cost_of_sales: Decimal,

    /// 売上総利益
    pub gross_profit: Decimal,

    /// 販売費及び一般管理費
    pub operating_expenses: Decimal,

    /// 営業利益
    pub operating_income: Decimal,

    /// 営業外収益
    pub non_operating_income: Decimal,

    /// 営業外費用
    pub non_operating_expenses: Decimal,

    /// 経常利益
    pub ordinary_income: Decimal,

    /// 当期純利益
    pub net_income: Decimal,
}
