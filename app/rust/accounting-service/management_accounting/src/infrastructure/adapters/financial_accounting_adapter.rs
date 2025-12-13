use reqwest::Client;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

use crate::domain::financial_data::FinancialData;
use shared::Result;

/// 財務会計サービスからのレスポンス DTO
#[derive(Debug, Deserialize, Serialize)]
pub struct JournalDto {
    pub journal_id: Option<i64>,
    pub journal_no: Option<String>,
    pub journal_date: String,
    pub fiscal_year: i32,
    pub entries: Vec<JournalEntryDto>,
}

#[derive(Debug, Deserialize, Serialize)]
pub struct JournalEntryDto {
    pub account_code: String,
    pub debit_amount: Decimal,
    pub credit_amount: Decimal,
}

/// 財務会計コンテキストとの統合アダプタ（腐敗防止層）
///
/// 財務会計コンテキストのデータモデルの変更から
/// 管理会計コンテキストを保護する
#[derive(Clone)]
pub struct FinancialAccountingAdapter {
    client: Client,
    base_url: String,
}

impl FinancialAccountingAdapter {
    pub fn new(base_url: String) -> Self {
        Self {
            client: Client::new(),
            base_url,
        }
    }

    /// 財務会計の仕訳データから管理会計用の財務データを抽出
    pub async fn fetch_financial_data_by_fiscal_year(
        &self,
        fiscal_year: i32,
    ) -> Result<FinancialData> {
        // 財務会計サービスから仕訳データを取得
        let url = format!("{}/api/v1/journals", self.base_url);
        let journals: Vec<JournalDto> = self
            .client
            .get(&url)
            .send()
            .await?
            .json()
            .await?;

        // 指定された会計年度の仕訳のみをフィルタリング
        let filtered_journals: Vec<JournalDto> = journals
            .into_iter()
            .filter(|j| j.fiscal_year == fiscal_year)
            .collect();

        // 財務会計のデータモデルを管理会計のドメインモデルに変換
        Ok(self.convert_to_financial_data(fiscal_year, filtered_journals))
    }

    /// 財務会計のデータモデルを管理会計のドメインモデルに変換
    ///
    /// この変換ロジックにより、財務会計のスキーマ変更の影響を局所化
    fn convert_to_financial_data(
        &self,
        fiscal_year: i32,
        journals: Vec<JournalDto>,
    ) -> FinancialData {
        let entries: Vec<&JournalEntryDto> = journals
            .iter()
            .flat_map(|j| &j.entries)
            .collect();

        // 勘定科目プレフィックスによる集計
        let sum_by_prefix = |prefix: &str, side: &str| -> Decimal {
            entries
                .iter()
                .filter(|e| e.account_code.starts_with(prefix))
                .map(|e| match side {
                    "debit" => e.debit_amount,
                    "credit" => e.credit_amount,
                    _ => Decimal::ZERO,
                })
                .sum()
        };

        let sum_by_code = |code: &str, side: &str| -> Decimal {
            entries
                .iter()
                .filter(|e| e.account_code == code)
                .map(|e| match side {
                    "debit" => e.debit_amount,
                    "credit" => e.credit_amount,
                    _ => Decimal::ZERO,
                })
                .sum()
        };

        // 売上高（41 から始まる勘定科目）
        let sales = sum_by_prefix("41", "credit");

        // 営業利益（簡易計算: 収益 - 費用）
        let revenue = sum_by_prefix("41", "credit");
        let cost_of_sales = sum_by_prefix("51", "debit");
        let selling_expenses = sum_by_prefix("52", "debit");
        let operating_profit = revenue - cost_of_sales - selling_expenses;

        // 総資産（1 から始まる勘定科目）
        let total_assets = sum_by_prefix("1", "debit");

        // 有形固定資産（121 から始まる勘定科目）
        let tangible_fixed_assets = sum_by_prefix("121", "debit");

        // 流動資産（11 から始まる勘定科目）
        let current_assets = sum_by_prefix("11", "debit");

        // 流動負債（21 から始まる勘定科目）
        let current_liabilities = sum_by_prefix("21", "credit");

        // 当座資産（現金預金 + 売掛金）
        let quick_assets = sum_by_code("111", "debit") + sum_by_code("112", "debit");

        // 自己資本（3 から始まる勘定科目）
        let equity = sum_by_prefix("3", "credit");

        FinancialData {
            fiscal_year,
            sales,
            operating_profit,
            total_assets,
            tangible_fixed_assets,
            current_assets,
            current_liabilities,
            quick_assets,
            equity,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rust_decimal_macros::dec;

    #[test]
    fn test_convert_to_financial_data() {
        let adapter = FinancialAccountingAdapter::new("http://localhost:8080".to_string());

        let journals = vec![
            JournalDto {
                journal_id: Some(1),
                journal_no: Some("J001".to_string()),
                journal_date: "2023-04-01".to_string(),
                fiscal_year: 2023,
                entries: vec![
                    JournalEntryDto {
                        account_code: "111".to_string(), // 現金預金
                        debit_amount: dec!(1000000),
                        credit_amount: dec!(0),
                    },
                    JournalEntryDto {
                        account_code: "411".to_string(), // 売上高
                        debit_amount: dec!(0),
                        credit_amount: dec!(1000000),
                    },
                ],
            },
        ];

        let financial_data = adapter.convert_to_financial_data(2023, journals);

        assert_eq!(financial_data.fiscal_year, 2023);
        assert_eq!(financial_data.sales, dec!(1000000));
    }
}
