use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

/// 仕訳（ヘッダー）
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Journal {
    /// 仕訳伝票番号
    pub journal_no: String,
    /// 起票日（実際の取引発生日）
    pub journal_date: NaiveDate,
    /// 入力日（システムへの入力日）
    pub input_date: NaiveDate,
    /// 決算仕訳フラグ（0=通常仕訳、1=決算仕訳）
    pub settlement_flag: i32,
    /// 単振フラグ（0=複合仕訳、1=単一仕訳）
    pub single_entry_flag: i32,
    /// 仕訳伝票区分
    pub journal_type: i32,
    /// 定期計上フラグ
    pub recurring_flag: i32,
    /// 社員コード
    pub employee_code: Option<String>,
    /// 部門コード
    pub department_code: Option<String>,
    /// 赤伝フラグ（0=通常、1=赤伝票（取消仕訳））
    pub reversal_flag: i32,
    /// 赤黒伝票番号
    pub reversal_journal_no: Option<String>,
    /// 仕訳明細
    pub details: Vec<JournalDetail>,
}

/// 仕訳明細
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JournalDetail {
    /// 仕訳伝票番号
    pub journal_no: String,
    /// 仕訳行番号
    pub line_number: i32,
    /// 行摘要
    pub description: String,
    /// 仕訳貸借明細
    pub items: Vec<JournalDebitCreditItem>,
}

/// 仕訳貸借明細
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JournalDebitCreditItem {
    /// 仕訳伝票番号
    pub journal_no: String,
    /// 仕訳行番号
    pub line_number: i32,
    /// 仕訳行貸借区分（D=借方、C=貸方）
    pub debit_credit_flag: String,
    /// 通貨コード
    pub currency_code: String,
    /// 為替レート
    pub exchange_rate: Decimal,
    /// 部門コード
    pub department_code: Option<String>,
    /// プロジェクトコード
    pub project_code: Option<String>,
    /// 勘定科目コード
    pub account_code: String,
    /// 補助科目コード
    pub sub_account_code: Option<String>,
    /// 仕訳金額
    pub amount: Decimal,
    /// 基軸換算仕訳金額
    pub base_amount: Decimal,
    /// 消費税区分
    pub tax_division: Option<String>,
    /// 消費税率
    pub tax_rate: Option<i32>,
    /// 消費税計算区分
    pub tax_calculation_type: Option<String>,
    /// 期日
    pub due_date: Option<NaiveDate>,
    /// 資金繰フラグ
    pub cash_flow_flag: i32,
    /// セグメントコード
    pub segment_code: Option<String>,
    /// 相手勘定科目コード
    pub contra_account_code: Option<String>,
    /// 相手補助科目コード
    pub contra_sub_account_code: Option<String>,
    /// 付箋コード
    pub note_code: Option<String>,
    /// 付箋内容
    pub note_content: Option<String>,
}

impl Journal {
    /// 新しい仕訳を作成
    pub fn new(
        journal_no: String,
        journal_date: NaiveDate,
        input_date: NaiveDate,
        settlement_flag: i32,
        single_entry_flag: i32,
    ) -> Self {
        Self {
            journal_no,
            journal_date,
            input_date,
            settlement_flag,
            single_entry_flag,
            journal_type: 0,
            recurring_flag: 0,
            employee_code: None,
            department_code: None,
            reversal_flag: 0,
            reversal_journal_no: None,
            details: Vec::new(),
        }
    }

    /// 仕訳明細を追加
    pub fn add_detail(&mut self, detail: JournalDetail) {
        self.details.push(detail);
    }

    /// 借方合計を計算
    pub fn calculate_debit_total(&self) -> Decimal {
        self.details
            .iter()
            .flat_map(|d| &d.items)
            .filter(|item| item.debit_credit_flag == "D")
            .map(|item| item.amount)
            .sum()
    }

    /// 貸方合計を計算
    pub fn calculate_credit_total(&self) -> Decimal {
        self.details
            .iter()
            .flat_map(|d| &d.items)
            .filter(|item| item.debit_credit_flag == "C")
            .map(|item| item.amount)
            .sum()
    }

    /// 借方・貸方のバランスを検証
    pub fn validate_balance(&self) -> Result<(), String> {
        let debit_total = self.calculate_debit_total();
        let credit_total = self.calculate_credit_total();

        if debit_total != credit_total {
            return Err(format!(
                "借方合計({})と貸方合計({})が一致しません",
                debit_total, credit_total
            ));
        }

        Ok(())
    }
}

impl JournalDetail {
    /// 新しい仕訳明細を作成
    pub fn new(journal_no: String, line_number: i32, description: String) -> Self {
        Self {
            journal_no,
            line_number,
            description,
            items: Vec::new(),
        }
    }

    /// 仕訳貸借明細を追加
    pub fn add_item(&mut self, item: JournalDebitCreditItem) {
        self.items.push(item);
    }
}

impl JournalDebitCreditItem {
    /// 新しい仕訳貸借明細を作成（最小限のパラメータ）
    #[allow(clippy::too_many_arguments)]
    pub fn new(
        journal_no: String,
        line_number: i32,
        debit_credit_flag: String,
        account_code: String,
        amount: Decimal,
    ) -> Self {
        Self {
            journal_no,
            line_number,
            debit_credit_flag,
            currency_code: "JPY".to_string(),
            exchange_rate: Decimal::ONE,
            department_code: None,
            project_code: None,
            account_code,
            sub_account_code: None,
            amount,
            base_amount: amount,
            tax_division: None,
            tax_rate: None,
            tax_calculation_type: None,
            due_date: None,
            cash_flow_flag: 0,
            segment_code: None,
            contra_account_code: None,
            contra_sub_account_code: None,
            note_code: None,
            note_content: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rust_decimal_macros::dec;

    #[test]
    fn test_journal_balance_validation() {
        // Given: 借方・貸方が一致する仕訳
        let mut journal = Journal::new(
            "JE-001".to_string(),
            NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            0,
            1,
        );

        let mut detail = JournalDetail::new("JE-001".to_string(), 1, "テスト仕訳".to_string());
        detail.add_item(JournalDebitCreditItem::new(
            "JE-001".to_string(),
            1,
            "D".to_string(),
            "1010".to_string(),
            dec!(100000),
        ));
        detail.add_item(JournalDebitCreditItem::new(
            "JE-001".to_string(),
            1,
            "C".to_string(),
            "4100".to_string(),
            dec!(100000),
        ));

        journal.add_detail(detail);

        // When: バランスを検証
        let result = journal.validate_balance();

        // Then: 成功する
        assert!(result.is_ok());
        assert_eq!(journal.calculate_debit_total(), dec!(100000));
        assert_eq!(journal.calculate_credit_total(), dec!(100000));
    }

    #[test]
    fn test_journal_balance_validation_failure() {
        // Given: 借方・貸方が一致しない仕訳
        let mut journal = Journal::new(
            "JE-002".to_string(),
            NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            NaiveDate::from_ymd_opt(2025, 1, 1).unwrap(),
            0,
            1,
        );

        let mut detail = JournalDetail::new("JE-002".to_string(), 1, "テスト仕訳".to_string());
        detail.add_item(JournalDebitCreditItem::new(
            "JE-002".to_string(),
            1,
            "D".to_string(),
            "1010".to_string(),
            dec!(100000),
        ));
        detail.add_item(JournalDebitCreditItem::new(
            "JE-002".to_string(),
            1,
            "C".to_string(),
            "4100".to_string(),
            dec!(99000),
        ));

        journal.add_detail(detail);

        // When: バランスを検証
        let result = journal.validate_balance();

        // Then: エラーになる
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("借方合計"));
    }
}
