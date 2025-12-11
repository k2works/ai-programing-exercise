use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::Serialize;
use utoipa::ToSchema;

use crate::domain::financial::balance_sheet::{BalanceSheet, BalanceSheetItem};
use crate::domain::financial::financial_ratios::FinancialRatios;
use crate::domain::financial::income_statement::{IncomeStatement, IncomeStatementItem};

/// 貸借対照表レスポンス
#[derive(Debug, Serialize, ToSchema)]
pub struct BalanceSheetResponse {
    pub as_of_date: NaiveDate,
    pub assets: Vec<BalanceSheetItemResponse>,
    pub liabilities: Vec<BalanceSheetItemResponse>,
    pub equity: Vec<BalanceSheetItemResponse>,
    pub total_assets: Decimal,
    pub total_liabilities: Decimal,
    pub total_equity: Decimal,
}

#[derive(Debug, Serialize, ToSchema)]
pub struct BalanceSheetItemResponse {
    pub account_code: String,
    pub account_name: String,
    pub balance: Decimal,
}

impl From<BalanceSheet> for BalanceSheetResponse {
    fn from(bs: BalanceSheet) -> Self {
        BalanceSheetResponse {
            as_of_date: bs.as_of_date,
            assets: bs
                .assets
                .into_iter()
                .map(BalanceSheetItemResponse::from)
                .collect(),
            liabilities: bs
                .liabilities
                .into_iter()
                .map(BalanceSheetItemResponse::from)
                .collect(),
            equity: bs
                .equity
                .into_iter()
                .map(BalanceSheetItemResponse::from)
                .collect(),
            total_assets: bs.total_assets,
            total_liabilities: bs.total_liabilities,
            total_equity: bs.total_equity,
        }
    }
}

impl From<BalanceSheetItem> for BalanceSheetItemResponse {
    fn from(item: BalanceSheetItem) -> Self {
        BalanceSheetItemResponse {
            account_code: item.account_code,
            account_name: item.account_name,
            balance: item.balance,
        }
    }
}

/// 損益計算書レスポンス
#[derive(Debug, Serialize, ToSchema)]
pub struct IncomeStatementResponse {
    pub start_date: NaiveDate,
    pub end_date: NaiveDate,
    pub revenues: Vec<IncomeStatementItemResponse>,
    pub expenses: Vec<IncomeStatementItemResponse>,
    pub total_sales: Decimal,
    pub cost_of_sales: Decimal,
    pub gross_profit: Decimal,
    pub operating_expenses: Decimal,
    pub operating_income: Decimal,
    pub non_operating_income: Decimal,
    pub non_operating_expenses: Decimal,
    pub ordinary_income: Decimal,
    pub net_income: Decimal,
}

#[derive(Debug, Serialize, ToSchema)]
pub struct IncomeStatementItemResponse {
    pub account_code: String,
    pub account_name: String,
    pub amount: Decimal,
    pub percentage: Decimal,
}

impl From<IncomeStatement> for IncomeStatementResponse {
    fn from(is: IncomeStatement) -> Self {
        IncomeStatementResponse {
            start_date: is.start_date,
            end_date: is.end_date,
            revenues: is
                .revenues
                .into_iter()
                .map(IncomeStatementItemResponse::from)
                .collect(),
            expenses: is
                .expenses
                .into_iter()
                .map(IncomeStatementItemResponse::from)
                .collect(),
            total_sales: is.total_sales,
            cost_of_sales: is.cost_of_sales,
            gross_profit: is.gross_profit,
            operating_expenses: is.operating_expenses,
            operating_income: is.operating_income,
            non_operating_income: is.non_operating_income,
            non_operating_expenses: is.non_operating_expenses,
            ordinary_income: is.ordinary_income,
            net_income: is.net_income,
        }
    }
}

impl From<IncomeStatementItem> for IncomeStatementItemResponse {
    fn from(item: IncomeStatementItem) -> Self {
        IncomeStatementItemResponse {
            account_code: item.account_code,
            account_name: item.account_name,
            amount: item.amount,
            percentage: item.percentage,
        }
    }
}

/// 財務指標レスポンス
#[derive(Debug, Serialize, ToSchema)]
pub struct FinancialRatiosResponse {
    pub current_ratio: Decimal,
    pub equity_ratio: Decimal,
    pub gross_profit_margin: Decimal,
    pub operating_profit_margin: Decimal,
    pub net_profit_margin: Decimal,
    pub roa: Decimal,
    pub roe: Decimal,
}

impl From<FinancialRatios> for FinancialRatiosResponse {
    fn from(fr: FinancialRatios) -> Self {
        FinancialRatiosResponse {
            current_ratio: fr.current_ratio,
            equity_ratio: fr.equity_ratio,
            gross_profit_margin: fr.gross_profit_margin,
            operating_profit_margin: fr.operating_profit_margin,
            net_profit_margin: fr.net_profit_margin,
            roa: fr.roa,
            roe: fr.roe,
        }
    }
}
