use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use utoipa::ToSchema;

use crate::domain::journal::{Journal, JournalDebitCreditItem, JournalDetail};

/// 仕訳作成リクエスト
#[derive(Debug, Deserialize, Serialize, ToSchema)]
pub struct JournalRequest {
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
    pub details: Vec<JournalDetailRequest>,
}

#[derive(Debug, Deserialize, Serialize, ToSchema)]
pub struct JournalDetailRequest {
    pub journal_no: String,
    pub line_number: i32,
    pub description: String,
    pub items: Vec<JournalDebitCreditItemRequest>,
}

#[derive(Debug, Deserialize, Serialize, ToSchema)]
pub struct JournalDebitCreditItemRequest {
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

impl From<JournalRequest> for Journal {
    fn from(req: JournalRequest) -> Self {
        Journal {
            journal_no: req.journal_no,
            journal_date: req.journal_date,
            input_date: req.input_date,
            settlement_flag: req.settlement_flag,
            single_entry_flag: req.single_entry_flag,
            journal_type: req.journal_type,
            recurring_flag: req.recurring_flag,
            employee_code: req.employee_code,
            department_code: req.department_code,
            reversal_flag: req.reversal_flag,
            reversal_journal_no: req.reversal_journal_no,
            details: req.details.into_iter().map(|d| d.into()).collect(),
        }
    }
}

impl From<JournalDetailRequest> for JournalDetail {
    fn from(req: JournalDetailRequest) -> Self {
        JournalDetail {
            journal_no: req.journal_no,
            line_number: req.line_number,
            description: req.description,
            items: req.items.into_iter().map(|i| i.into()).collect(),
        }
    }
}

impl From<JournalDebitCreditItemRequest> for JournalDebitCreditItem {
    fn from(req: JournalDebitCreditItemRequest) -> Self {
        JournalDebitCreditItem {
            journal_no: req.journal_no,
            line_number: req.line_number,
            debit_credit_flag: req.debit_credit_flag,
            currency_code: req.currency_code,
            exchange_rate: req.exchange_rate,
            department_code: req.department_code,
            project_code: req.project_code,
            account_code: req.account_code,
            sub_account_code: req.sub_account_code,
            amount: req.amount,
            base_amount: req.base_amount,
            tax_division: req.tax_division,
            tax_rate: req.tax_rate,
            tax_calculation_type: req.tax_calculation_type,
            due_date: req.due_date,
            cash_flow_flag: req.cash_flow_flag,
            segment_code: req.segment_code,
            contra_account_code: req.contra_account_code,
            contra_sub_account_code: req.contra_sub_account_code,
            note_code: req.note_code,
            note_content: req.note_content,
        }
    }
}
