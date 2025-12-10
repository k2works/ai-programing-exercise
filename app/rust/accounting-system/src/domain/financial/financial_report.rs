use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

use super::balance_sheet::BalanceSheet;
use super::financial_ratios::FinancialRatios;
use super::income_statement::IncomeStatement;

/// 財務分析レポート
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FinancialReport {
    /// レポート作成日
    pub report_date: NaiveDate,

    /// 貸借対照表
    pub balance_sheet: BalanceSheet,

    /// 損益計算書
    pub income_statement: IncomeStatement,

    /// 財務指標
    pub financial_ratios: FinancialRatios,

    /// 分析コメント
    pub analysis_comments: Vec<String>,
}

impl FinancialReport {
    /// 分析コメントを生成
    pub fn generate_analysis_comments(&self) -> Vec<String> {
        let mut comments = Vec::new();

        // 安全性分析
        if self.financial_ratios.current_ratio < Decimal::from(100) {
            comments.push(format!(
                "流動比率が{}%で100%を下回っており、短期的な支払能力に注意が必要です。",
                self.financial_ratios.current_ratio
            ));
        } else {
            comments.push(format!(
                "流動比率が{}%で良好な状態です。",
                self.financial_ratios.current_ratio
            ));
        }

        if self.financial_ratios.equity_ratio < Decimal::from(30) {
            comments.push(format!(
                "自己資本比率が{}%で低水準です。財務健全性の改善が推奨されます。",
                self.financial_ratios.equity_ratio
            ));
        } else {
            comments.push(format!(
                "自己資本比率が{}%で健全な水準です。",
                self.financial_ratios.equity_ratio
            ));
        }

        // 収益性分析
        if self.financial_ratios.net_profit_margin < Decimal::from(5) {
            comments.push(format!(
                "売上高純利益率が{}%で低水準です。収益性の改善が必要です。",
                self.financial_ratios.net_profit_margin
            ));
        } else {
            comments.push(format!(
                "売上高純利益率が{}%で良好です。",
                self.financial_ratios.net_profit_margin
            ));
        }

        comments
    }
}
