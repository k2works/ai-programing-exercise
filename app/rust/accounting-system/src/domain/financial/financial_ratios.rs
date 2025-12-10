use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

/// 財務指標
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FinancialRatios {
    // 安全性指標
    /// 流動比率（%）= 流動資産 / 流動負債 × 100
    pub current_ratio: Decimal,

    /// 自己資本比率（%）= 自己資本 / 総資産 × 100
    pub equity_ratio: Decimal,

    // 収益性指標
    /// 売上高総利益率（%）= 売上総利益 / 売上高 × 100
    pub gross_profit_margin: Decimal,

    /// 売上高営業利益率（%）= 営業利益 / 売上高 × 100
    pub operating_profit_margin: Decimal,

    /// 売上高純利益率（%）= 当期純利益 / 売上高 × 100
    pub net_profit_margin: Decimal,

    /// ROA（%）= 当期純利益 / 総資産 × 100
    pub roa: Decimal,

    /// ROE（%）= 当期純利益 / 自己資本 × 100
    pub roe: Decimal,
}
