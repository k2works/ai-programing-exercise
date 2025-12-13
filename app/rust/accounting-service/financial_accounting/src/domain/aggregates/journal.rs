use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::domain::events::journal_events::*;

/// 仕訳ステータス
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum JournalStatus {
    /// 下書き
    Draft,
    /// 承認済み
    Approved,
    /// 取消済み
    Canceled,
}

/// 仕訳明細
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JournalEntry {
    /// 明細ID
    pub entry_id: i64,
    /// 勘定科目コード
    pub account_code: String,
    /// 借方/貸方フラグ
    pub debit_credit: String,
    /// 金額
    pub amount: Decimal,
    /// 摘要
    pub description: String,
}

/// 仕訳集約（イベントソーシング版）
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JournalAggregate {
    /// 仕訳ID
    pub journal_id: String,
    /// 起票日
    pub journal_date: NaiveDate,
    /// 摘要
    pub description: String,
    /// 仕訳明細（明細ID -> 明細）
    pub entries: HashMap<i64, JournalEntry>,
    /// ステータス
    pub status: JournalStatus,
}

impl JournalAggregate {
    /// 新しい仕訳集約を作成
    pub fn new() -> Self {
        Self {
            journal_id: String::new(),
            journal_date: NaiveDate::default(),
            description: String::new(),
            entries: HashMap::new(),
            status: JournalStatus::Draft,
        }
    }

    /// イベントを適用して状態を更新
    pub fn apply_event(&mut self, event: &JournalEvent) {
        match event {
            JournalEvent::Created(data) => {
                self.journal_id = data.journal_id.clone();
                self.journal_date = data.journal_date;
                self.description = data.description.clone();
                self.status = JournalStatus::Draft;
            }
            JournalEvent::EntryAdded(data) => {
                self.entries.insert(
                    data.entry_id,
                    JournalEntry {
                        entry_id: data.entry_id,
                        account_code: data.account_code.clone(),
                        debit_credit: data.debit_credit.clone(),
                        amount: data.amount,
                        description: data.description.clone(),
                    },
                );
            }
            JournalEvent::EntryRemoved(data) => {
                self.entries.remove(&data.entry_id);
            }
            JournalEvent::Approved(_) => {
                self.status = JournalStatus::Approved;
            }
            JournalEvent::Canceled(_) => {
                self.status = JournalStatus::Canceled;
            }
        }
    }

    /// イベント列から集約を再構築
    pub fn from_events(events: Vec<JournalEvent>) -> Self {
        let mut aggregate = Self::new();
        for event in events {
            aggregate.apply_event(&event);
        }
        aggregate
    }

    /// 借方合計を計算
    pub fn calculate_debit_total(&self) -> Decimal {
        self.entries
            .values()
            .filter(|e| e.debit_credit == "D")
            .map(|e| e.amount)
            .sum()
    }

    /// 貸方合計を計算
    pub fn calculate_credit_total(&self) -> Decimal {
        self.entries
            .values()
            .filter(|e| e.debit_credit == "C")
            .map(|e| e.amount)
            .sum()
    }

    /// 借方・貸方のバランスを検証
    pub fn is_balanced(&self) -> bool {
        self.calculate_debit_total() == self.calculate_credit_total()
    }

    /// 承認可能かチェック
    pub fn can_approve(&self) -> Result<(), String> {
        if self.status != JournalStatus::Draft {
            return Err("下書き状態の仕訳のみ承認できます".to_string());
        }

        if !self.is_balanced() {
            return Err("借方と貸方の合計が一致していません".to_string());
        }

        if self.entries.is_empty() {
            return Err("仕訳明細が空です".to_string());
        }

        Ok(())
    }
}

impl Default for JournalAggregate {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use rust_decimal_macros::dec;

    #[test]
    fn test_apply_created_event() {
        // Given: 仕訳作成イベント
        let event = JournalEvent::Created(JournalCreatedData {
            journal_id: "J-001".to_string(),
            journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
            description: "売掛金の回収".to_string(),
        });

        // When: イベントを適用
        let mut aggregate = JournalAggregate::new();
        aggregate.apply_event(&event);

        // Then: 状態が更新される
        assert_eq!(aggregate.journal_id, "J-001");
        assert_eq!(
            aggregate.journal_date,
            NaiveDate::from_ymd_opt(2024, 1, 15).unwrap()
        );
        assert_eq!(aggregate.description, "売掛金の回収");
        assert_eq!(aggregate.status, JournalStatus::Draft);
    }

    #[test]
    fn test_apply_entry_added_event() {
        // Given: 仕訳明細追加イベント
        let event = JournalEvent::EntryAdded(EntryAddedData {
            entry_id: 1,
            account_code: "1010".to_string(),
            debit_credit: "D".to_string(),
            amount: dec!(100000),
            description: "現金".to_string(),
        });

        // When: イベントを適用
        let mut aggregate = JournalAggregate::new();
        aggregate.apply_event(&event);

        // Then: 明細が追加される
        assert_eq!(aggregate.entries.len(), 1);
        let entry = aggregate.entries.get(&1).unwrap();
        assert_eq!(entry.account_code, "1010");
        assert_eq!(entry.amount, dec!(100000));
    }

    #[test]
    fn test_apply_entry_removed_event() {
        // Given: 明細が存在する集約
        let mut aggregate = JournalAggregate::new();
        aggregate.apply_event(&JournalEvent::EntryAdded(EntryAddedData {
            entry_id: 1,
            account_code: "1010".to_string(),
            debit_credit: "D".to_string(),
            amount: dec!(100000),
            description: "現金".to_string(),
        }));

        // When: 明細削除イベントを適用
        let event = JournalEvent::EntryRemoved(EntryRemovedData { entry_id: 1 });
        aggregate.apply_event(&event);

        // Then: 明細が削除される
        assert_eq!(aggregate.entries.len(), 0);
    }

    #[test]
    fn test_from_events() {
        // Given: 複数のイベント
        let events = vec![
            JournalEvent::Created(JournalCreatedData {
                journal_id: "J-001".to_string(),
                journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
                description: "テスト仕訳".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 1,
                account_code: "1010".to_string(),
                debit_credit: "D".to_string(),
                amount: dec!(100000),
                description: "現金".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 2,
                account_code: "4100".to_string(),
                debit_credit: "C".to_string(),
                amount: dec!(100000),
                description: "売上".to_string(),
            }),
        ];

        // When: イベントから集約を再構築
        let aggregate = JournalAggregate::from_events(events);

        // Then: 正しい状態になる
        assert_eq!(aggregate.journal_id, "J-001");
        assert_eq!(aggregate.entries.len(), 2);
        assert_eq!(aggregate.calculate_debit_total(), dec!(100000));
        assert_eq!(aggregate.calculate_credit_total(), dec!(100000));
        assert!(aggregate.is_balanced());
    }

    #[test]
    fn test_is_balanced() {
        // Given: バランスした仕訳
        let events = vec![
            JournalEvent::Created(JournalCreatedData {
                journal_id: "J-001".to_string(),
                journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
                description: "テスト仕訳".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 1,
                account_code: "1010".to_string(),
                debit_credit: "D".to_string(),
                amount: dec!(100000),
                description: "現金".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 2,
                account_code: "4100".to_string(),
                debit_credit: "C".to_string(),
                amount: dec!(100000),
                description: "売上".to_string(),
            }),
        ];

        let aggregate = JournalAggregate::from_events(events);

        // Then: バランスしている
        assert!(aggregate.is_balanced());
    }

    #[test]
    fn test_is_not_balanced() {
        // Given: バランスしていない仕訳
        let events = vec![
            JournalEvent::Created(JournalCreatedData {
                journal_id: "J-001".to_string(),
                journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
                description: "テスト仕訳".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 1,
                account_code: "1010".to_string(),
                debit_credit: "D".to_string(),
                amount: dec!(100000),
                description: "現金".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 2,
                account_code: "4100".to_string(),
                debit_credit: "C".to_string(),
                amount: dec!(50000),
                description: "売上".to_string(),
            }),
        ];

        let aggregate = JournalAggregate::from_events(events);

        // Then: バランスしていない
        assert!(!aggregate.is_balanced());
    }

    #[test]
    fn test_approve_event() {
        // Given: バランスした仕訳
        let events = vec![
            JournalEvent::Created(JournalCreatedData {
                journal_id: "J-001".to_string(),
                journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
                description: "テスト仕訳".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 1,
                account_code: "1010".to_string(),
                debit_credit: "D".to_string(),
                amount: dec!(100000),
                description: "現金".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 2,
                account_code: "4100".to_string(),
                debit_credit: "C".to_string(),
                amount: dec!(100000),
                description: "売上".to_string(),
            }),
        ];

        let mut aggregate = JournalAggregate::from_events(events);

        // When: 承認イベントを適用
        let event = JournalEvent::Approved(JournalApprovedData {
            approved_by: "manager@example.com".to_string(),
            approved_at: Utc::now(),
        });
        aggregate.apply_event(&event);

        // Then: ステータスが承認済みになる
        assert_eq!(aggregate.status, JournalStatus::Approved);
    }

    #[test]
    fn test_can_approve_valid() {
        // Given: バランスした下書き仕訳
        let events = vec![
            JournalEvent::Created(JournalCreatedData {
                journal_id: "J-001".to_string(),
                journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
                description: "テスト仕訳".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 1,
                account_code: "1010".to_string(),
                debit_credit: "D".to_string(),
                amount: dec!(100000),
                description: "現金".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 2,
                account_code: "4100".to_string(),
                debit_credit: "C".to_string(),
                amount: dec!(100000),
                description: "売上".to_string(),
            }),
        ];

        let aggregate = JournalAggregate::from_events(events);

        // Then: 承認可能
        assert!(aggregate.can_approve().is_ok());
    }

    #[test]
    fn test_can_approve_not_balanced() {
        // Given: バランスしていない仕訳
        let events = vec![
            JournalEvent::Created(JournalCreatedData {
                journal_id: "J-001".to_string(),
                journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
                description: "テスト仕訳".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 1,
                account_code: "1010".to_string(),
                debit_credit: "D".to_string(),
                amount: dec!(100000),
                description: "現金".to_string(),
            }),
        ];

        let aggregate = JournalAggregate::from_events(events);

        // Then: 承認不可
        let result = aggregate.can_approve();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("一致していません"));
    }

    #[test]
    fn test_can_approve_already_approved() {
        // Given: すでに承認済みの仕訳
        let events = vec![
            JournalEvent::Created(JournalCreatedData {
                journal_id: "J-001".to_string(),
                journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
                description: "テスト仕訳".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 1,
                account_code: "1010".to_string(),
                debit_credit: "D".to_string(),
                amount: dec!(100000),
                description: "現金".to_string(),
            }),
            JournalEvent::EntryAdded(EntryAddedData {
                entry_id: 2,
                account_code: "4100".to_string(),
                debit_credit: "C".to_string(),
                amount: dec!(100000),
                description: "売上".to_string(),
            }),
            JournalEvent::Approved(JournalApprovedData {
                approved_by: "manager@example.com".to_string(),
                approved_at: Utc::now(),
            }),
        ];

        let aggregate = JournalAggregate::from_events(events);

        // Then: 承認不可
        let result = aggregate.can_approve();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("下書き状態"));
    }
}
