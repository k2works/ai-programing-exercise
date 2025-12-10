use chrono::NaiveDate;
use rust_decimal::prelude::Zero;
use rust_decimal::Decimal;
use sqlx::PgPool;

use crate::domain::financial::balance_sheet::{BalanceSheet, BalanceSheetItem};
use crate::domain::financial::financial_ratios::FinancialRatios;
use crate::domain::financial::income_statement::{IncomeStatement, IncomeStatementItem};

/// 財務諸表生成サービス
pub struct FinancialStatementService {
    pool: PgPool,
}

impl FinancialStatementService {
    /// コンストラクタ
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    /// 貸借対照表を生成
    ///
    /// # Arguments
    ///
    /// * `as_of_date` - 基準日
    pub async fn generate_balance_sheet(
        &self,
        as_of_date: NaiveDate,
    ) -> Result<BalanceSheet, sqlx::Error> {
        // 資産・負債・純資産の残高を取得
        let sql = r#"
            SELECT
                a."勘定科目コード" as account_code,
                a."勘定科目名" as account_name,
                a."BSPL区分" as bspl_type,
                a."取引要素区分" as element_type,
                COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0) as balance
            FROM "勘定科目マスタ" a
            LEFT JOIN "日次勘定科目残高" d
                ON a."勘定科目コード" = d."勘定科目コード"
            WHERE a."BSPL区分" = 'B'
                AND (d."起票日" <= $1 OR d."起票日" IS NULL)
            GROUP BY a."勘定科目コード", a."勘定科目名", a."BSPL区分", a."取引要素区分"
            HAVING COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0) != 0
            ORDER BY a."勘定科目コード"
        "#;

        #[derive(sqlx::FromRow)]
        struct BalanceRow {
            account_code: String,
            account_name: String,
            balance: Decimal,
            element_type: String,
        }

        let balances: Vec<BalanceRow> = sqlx::query_as(sql)
            .bind(as_of_date)
            .fetch_all(&self.pool)
            .await?;

        let mut assets = Vec::new();
        let mut liabilities = Vec::new();
        let mut equity = Vec::new();

        // データを分類
        for row in balances {
            let item = BalanceSheetItem {
                account_code: row.account_code,
                account_name: row.account_name,
                balance: row.balance.abs(),
                percentage: Decimal::zero(), // 後で計算
            };

            match row.element_type.as_str() {
                "資産" => assets.push(item),
                "負債" => liabilities.push(item),
                "資本" | "純資産" => equity.push(item),
                _ => {}
            }
        }

        // 合計を計算
        let total_assets: Decimal = assets.iter().map(|item| item.balance).sum();
        let total_liabilities: Decimal = liabilities.iter().map(|item| item.balance).sum();
        let total_equity: Decimal = equity.iter().map(|item| item.balance).sum();
        let total_liabilities_and_equity = total_liabilities + total_equity;

        // 構成比率を計算
        let assets_with_percentage = Self::calculate_percentage(assets, total_assets);
        let liabilities_with_percentage =
            Self::calculate_percentage(liabilities, total_liabilities_and_equity);
        let equity_with_percentage =
            Self::calculate_percentage(equity, total_liabilities_and_equity);

        Ok(BalanceSheet {
            as_of_date,
            assets: assets_with_percentage,
            liabilities: liabilities_with_percentage,
            equity: equity_with_percentage,
            total_assets,
            total_liabilities,
            total_equity,
            total_liabilities_and_equity,
        })
    }

    /// 損益計算書を生成
    ///
    /// # Arguments
    ///
    /// * `start_date` - 期間開始日
    /// * `end_date` - 期間終了日
    pub async fn generate_income_statement(
        &self,
        start_date: NaiveDate,
        end_date: NaiveDate,
    ) -> Result<IncomeStatement, sqlx::Error> {
        // 収益・費用の残高を取得
        let sql = r#"
            SELECT
                a."勘定科目コード" as account_code,
                a."勘定科目名" as account_name,
                a."BSPL区分" as bspl_type,
                a."取引要素区分" as element_type,
                COALESCE(a."費用区分", '0') as expense_type,
                COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0) as balance
            FROM "勘定科目マスタ" a
            LEFT JOIN "日次勘定科目残高" d
                ON a."勘定科目コード" = d."勘定科目コード"
            WHERE a."BSPL区分" = 'P'
                AND d."起票日" BETWEEN $1 AND $2
            GROUP BY a."勘定科目コード", a."勘定科目名", a."BSPL区分", a."取引要素区分", a."費用区分"
            HAVING COALESCE(SUM(d."借方金額"), 0) - COALESCE(SUM(d."貸方金額"), 0) != 0
            ORDER BY a."勘定科目コード"
        "#;

        #[derive(sqlx::FromRow)]
        struct ProfitLossRow {
            account_code: String,
            account_name: String,
            balance: Decimal,
            element_type: String,
            expense_type: String,
        }

        let balances: Vec<ProfitLossRow> = sqlx::query_as(sql)
            .bind(start_date)
            .bind(end_date)
            .fetch_all(&self.pool)
            .await?;

        let mut revenues = Vec::new();
        let mut expenses = Vec::new();
        let mut cost_of_sales = Decimal::zero();
        let mut operating_expenses = Decimal::zero();

        // データを分類
        for row in balances {
            let item = IncomeStatementItem {
                account_code: row.account_code.clone(),
                account_name: row.account_name,
                amount: row.balance.abs(),
                percentage: Decimal::zero(),
            };

            match row.element_type.as_str() {
                "収益" => revenues.push(item),
                "費用" => {
                    // 費用区分で売上原価と販管費を区別
                    if row.expense_type == "1" {
                        cost_of_sales += item.amount;
                    } else {
                        operating_expenses += item.amount;
                    }
                    expenses.push(item);
                }
                _ => {}
            }
        }

        // 利益項目を計算
        let total_sales: Decimal = revenues.iter().map(|r| r.amount).sum();
        let gross_profit = total_sales - cost_of_sales;
        let operating_income = gross_profit - operating_expenses;
        let ordinary_income = operating_income; // 簡略化
        let net_income = ordinary_income; // 簡略化

        // 構成比率を計算
        let revenues_with_percentage = Self::calculate_pl_percentage(revenues, total_sales);
        let expenses_with_percentage = Self::calculate_pl_percentage(expenses, total_sales);

        Ok(IncomeStatement {
            start_date,
            end_date,
            revenues: revenues_with_percentage,
            expenses: expenses_with_percentage,
            total_sales,
            cost_of_sales,
            gross_profit,
            operating_expenses,
            operating_income,
            non_operating_income: Decimal::zero(),
            non_operating_expenses: Decimal::zero(),
            ordinary_income,
            net_income,
        })
    }

    /// 構成比率を計算（貸借対照表用）
    ///
    /// # Arguments
    ///
    /// * `items` - 項目リスト
    /// * `total` - 合計額
    fn calculate_percentage(items: Vec<BalanceSheetItem>, total: Decimal) -> Vec<BalanceSheetItem> {
        items
            .into_iter()
            .map(|item| {
                let percentage = if total > Decimal::zero() {
                    ((item.balance / total) * Decimal::from(100)).round_dp_with_strategy(
                        2,
                        rust_decimal::RoundingStrategy::MidpointAwayFromZero,
                    )
                } else {
                    Decimal::zero()
                };

                BalanceSheetItem { percentage, ..item }
            })
            .collect()
    }

    /// 財務指標を計算
    ///
    /// # Arguments
    ///
    /// * `balance_sheet` - 貸借対照表
    /// * `income_statement` - 損益計算書
    pub fn calculate_financial_ratios(
        &self,
        balance_sheet: &BalanceSheet,
        income_statement: &IncomeStatement,
    ) -> FinancialRatios {
        // 流動資産・流動負債を計算（勘定科目コードで判定）
        let current_assets: Decimal = balance_sheet
            .assets
            .iter()
            .filter(|a| a.account_code.starts_with("11"))
            .map(|a| a.balance)
            .sum();

        let current_liabilities: Decimal = balance_sheet
            .liabilities
            .iter()
            .filter(|l| l.account_code.starts_with("21"))
            .map(|l| l.balance)
            .sum();

        // 安全性指標
        let current_ratio = if current_liabilities > Decimal::zero() {
            (current_assets / current_liabilities * Decimal::from(100)).round_dp(2)
        } else {
            Decimal::zero()
        };

        let equity_ratio = if balance_sheet.total_assets > Decimal::zero() {
            (balance_sheet.total_equity / balance_sheet.total_assets * Decimal::from(100))
                .round_dp(2)
        } else {
            Decimal::zero()
        };

        // 収益性指標
        let gross_profit_margin = if income_statement.total_sales > Decimal::zero() {
            (income_statement.gross_profit / income_statement.total_sales * Decimal::from(100))
                .round_dp(2)
        } else {
            Decimal::zero()
        };

        let operating_profit_margin = if income_statement.total_sales > Decimal::zero() {
            (income_statement.operating_income / income_statement.total_sales * Decimal::from(100))
                .round_dp(2)
        } else {
            Decimal::zero()
        };

        let net_profit_margin = if income_statement.total_sales > Decimal::zero() {
            (income_statement.net_income / income_statement.total_sales * Decimal::from(100))
                .round_dp(2)
        } else {
            Decimal::zero()
        };

        let roa = if balance_sheet.total_assets > Decimal::zero() {
            (income_statement.net_income / balance_sheet.total_assets * Decimal::from(100))
                .round_dp(2)
        } else {
            Decimal::zero()
        };

        let roe = if balance_sheet.total_equity > Decimal::zero() {
            (income_statement.net_income / balance_sheet.total_equity * Decimal::from(100))
                .round_dp(2)
        } else {
            Decimal::zero()
        };

        FinancialRatios {
            current_ratio,
            equity_ratio,
            gross_profit_margin,
            operating_profit_margin,
            net_profit_margin,
            roa,
            roe,
        }
    }

    /// 構成比率を計算（損益計算書用）
    ///
    /// # Arguments
    ///
    /// * `items` - 項目リスト
    /// * `total_sales` - 売上高
    fn calculate_pl_percentage(
        items: Vec<IncomeStatementItem>,
        total_sales: Decimal,
    ) -> Vec<IncomeStatementItem> {
        items
            .into_iter()
            .map(|item| {
                let percentage = if total_sales > Decimal::zero() {
                    ((item.amount / total_sales) * Decimal::from(100)).round_dp_with_strategy(
                        2,
                        rust_decimal::RoundingStrategy::MidpointAwayFromZero,
                    )
                } else {
                    Decimal::zero()
                };

                IncomeStatementItem { percentage, ..item }
            })
            .collect()
    }
}
