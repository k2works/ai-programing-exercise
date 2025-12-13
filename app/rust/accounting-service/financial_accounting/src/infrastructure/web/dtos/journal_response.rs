use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

use crate::domain::journal::{Journal, JournalDebitCreditItem, JournalDetail};

/// 仕訳レスポンス
#[derive(Debug, Serialize, Deserialize, ToSchema)]
pub struct JournalResponse {
    pub journal_no: String,
    pub journal_date: NaiveDate,
    pub input_date: NaiveDate,
    pub settlement_flag: i32,
    pub single_entry_flag: i32,
    pub journal_type: i32,
    pub recurring_flag: i32,
    pub employee_code: Option<String>,
    pub department_code: Option<String>,
    pub reversal_flag: i32,
    pub reversal_journal_no: Option<String>,
    pub details: Vec<JournalDetailResponse>,
}

#[derive(Debug, Serialize, Deserialize, ToSchema)]
pub struct JournalDetailResponse {
    pub journal_no: String,
    pub line_number: i32,
    pub description: String,
    pub items: Vec<JournalDebitCreditItemResponse>,
}

#[derive(Debug, Serialize, Deserialize, ToSchema)]
pub struct JournalDebitCreditItemResponse {
    pub journal_no: String,
    pub line_number: i32,
    pub debit_credit_flag: String,
    pub currency_code: String,
    pub exchange_rate: Decimal,
    pub department_code: Option<String>,
    pub project_code: Option<String>,
    pub account_code: String,
    pub sub_account_code: Option<String>,
    pub amount: Decimal,
    pub base_amount: Decimal,
    pub tax_division: Option<String>,
    pub tax_rate: Option<i32>,
    pub tax_calculation_type: Option<String>,
    pub due_date: Option<NaiveDate>,
    pub cash_flow_flag: i32,
    pub segment_code: Option<String>,
    pub contra_account_code: Option<String>,
    pub contra_sub_account_code: Option<String>,
    pub note_code: Option<String>,
    pub note_content: Option<String>,
}

impl From<Journal> for JournalResponse {
    fn from(journal: Journal) -> Self {
        JournalResponse {
            journal_no: journal.journal_no,
            journal_date: journal.journal_date,
            input_date: journal.input_date,
            settlement_flag: journal.settlement_flag,
            single_entry_flag: journal.single_entry_flag,
            journal_type: journal.journal_type,
            recurring_flag: journal.recurring_flag,
            employee_code: journal.employee_code,
            department_code: journal.department_code,
            reversal_flag: journal.reversal_flag,
            reversal_journal_no: journal.reversal_journal_no,
            details: journal.details.into_iter().map(|d| d.into()).collect(),
        }
    }
}

impl From<JournalDetail> for JournalDetailResponse {
    fn from(detail: JournalDetail) -> Self {
        JournalDetailResponse {
            journal_no: detail.journal_no,
            line_number: detail.line_number,
            description: detail.description,
            items: detail.items.into_iter().map(|i| i.into()).collect(),
        }
    }
}

impl From<JournalDebitCreditItem> for JournalDebitCreditItemResponse {
    fn from(item: JournalDebitCreditItem) -> Self {
        JournalDebitCreditItemResponse {
            journal_no: item.journal_no,
            line_number: item.line_number,
            debit_credit_flag: item.debit_credit_flag,
            currency_code: item.currency_code,
            exchange_rate: item.exchange_rate,
            department_code: item.department_code,
            project_code: item.project_code,
            account_code: item.account_code,
            sub_account_code: item.sub_account_code,
            amount: item.amount,
            base_amount: item.base_amount,
            tax_division: item.tax_division,
            tax_rate: item.tax_rate,
            tax_calculation_type: item.tax_calculation_type,
            due_date: item.due_date,
            cash_flow_flag: item.cash_flow_flag,
            segment_code: item.segment_code,
            contra_account_code: item.contra_account_code,
            contra_sub_account_code: item.contra_sub_account_code,
            note_code: item.note_code,
            note_content: item.note_content,
        }
    }
}
