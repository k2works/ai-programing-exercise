use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

/// 管理会計コンテキストは財務会計コンテキストのデータを参照する
/// 独自のテーブルは持たず、財務会計の Journal と JournalEntry から
/// 必要なデータを抽出して分析する
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FinancialData {
    pub fiscal_year: i32,
    pub sales: Decimal,                      // 売上高
    pub operating_profit: Decimal,           // 営業利益
    pub total_assets: Decimal,               // 総資産
    pub tangible_fixed_assets: Decimal,      // 有形固定資産
    pub current_assets: Decimal,             // 流動資産
    pub current_liabilities: Decimal,        // 流動負債
    pub quick_assets: Decimal,               // 当座資産
    pub equity: Decimal,                     // 自己資本
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FinancialRatios {
    pub operating_profit_margin: Decimal,    // 営業利益率
    pub total_asset_turnover: Decimal,       // 総資産回転率
    pub tangible_fixed_asset_turnover: Decimal, // 有形固定資産回転率
    pub current_ratio: Decimal,              // 流動比率
    pub quick_ratio: Decimal,                // 当座比率
    pub equity_ratio: Decimal,               // 自己資本比率
}

impl FinancialData {
    /// 財務比率の計算
    pub fn calculate_ratios(&self) -> Result<FinancialRatios, String> {
        if self.sales.is_zero() {
            return Err("売上高が0のため比率計算不可".to_string());
        }
        if self.total_assets.is_zero() {
            return Err("総資産が0のため比率計算不可".to_string());
        }
        if self.current_liabilities.is_zero() {
            return Err("流動負債が0のため比率計算不可".to_string());
        }
        if self.tangible_fixed_assets.is_zero() {
            return Err("有形固定資産が0のため比率計算不可".to_string());
        }

        Ok(FinancialRatios {
            operating_profit_margin: (self.operating_profit / self.sales) * Decimal::from(100),
            total_asset_turnover: self.sales / self.total_assets,
            tangible_fixed_asset_turnover: self.sales / self.tangible_fixed_assets,
            current_ratio: (self.current_assets / self.current_liabilities) * Decimal::from(100),
            quick_ratio: (self.quick_assets / self.current_liabilities) * Decimal::from(100),
            equity_ratio: (self.equity / self.total_assets) * Decimal::from(100),
        })
    }
}
