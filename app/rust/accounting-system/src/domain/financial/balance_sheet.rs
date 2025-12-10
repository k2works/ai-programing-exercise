use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

/// 貸借対照表の項目
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BalanceSheetItem {
    /// 勘定科目コード
    pub account_code: String,

    /// 勘定科目名
    pub account_name: String,

    /// 残高
    pub balance: Decimal,

    /// 構成比率（%）
    pub percentage: Decimal,
}

/// 貸借対照表
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BalanceSheet {
    /// 基準日
    pub as_of_date: NaiveDate,

    /// 資産項目
    pub assets: Vec<BalanceSheetItem>,

    /// 負債項目
    pub liabilities: Vec<BalanceSheetItem>,

    /// 純資産項目
    pub equity: Vec<BalanceSheetItem>,

    /// 資産合計
    pub total_assets: Decimal,

    /// 負債合計
    pub total_liabilities: Decimal,

    /// 純資産合計
    pub total_equity: Decimal,

    /// 負債・純資産合計
    pub total_liabilities_and_equity: Decimal,
}
