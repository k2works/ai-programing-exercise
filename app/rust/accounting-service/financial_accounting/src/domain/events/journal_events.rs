use chrono::NaiveDate;
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};

/// 仕訳イベント
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type", content = "data")]
pub enum JournalEvent {
    /// 仕訳作成イベント
    Created(JournalCreatedData),
    /// 仕訳明細追加イベント
    EntryAdded(EntryAddedData),
    /// 仕訳明細削除イベント
    EntryRemoved(EntryRemovedData),
    /// 仕訳承認イベント
    Approved(JournalApprovedData),
    /// 仕訳取消イベント
    Canceled(JournalCanceledData),
}

/// 仕訳作成データ
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JournalCreatedData {
    /// 仕訳ID
    pub journal_id: String,
    /// 起票日
    pub journal_date: NaiveDate,
    /// 摘要
    pub description: String,
}

/// 仕訳明細追加データ
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EntryAddedData {
    /// 明細ID
    pub entry_id: i64,
    /// 勘定科目コード
    pub account_code: String,
    /// 借方/貸方フラグ（D=借方、C=貸方）
    pub debit_credit: String,
    /// 金額
    pub amount: Decimal,
    /// 摘要
    pub description: String,
}

/// 仕訳明細削除データ
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EntryRemovedData {
    /// 明細ID
    pub entry_id: i64,
}

/// 仕訳承認データ
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JournalApprovedData {
    /// 承認者
    pub approved_by: String,
    /// 承認日時
    pub approved_at: chrono::DateTime<chrono::Utc>,
}

/// 仕訳取消データ
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct JournalCanceledData {
    /// 取消理由
    pub reason: String,
    /// 取消者
    pub canceled_by: String,
    /// 取消日時
    pub canceled_at: chrono::DateTime<chrono::Utc>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::Utc;
    use rust_decimal_macros::dec;

    #[test]
    fn test_journal_event_serialization() {
        // Given: 仕訳作成イベント
        let event = JournalEvent::Created(JournalCreatedData {
            journal_id: "J-001".to_string(),
            journal_date: NaiveDate::from_ymd_opt(2024, 1, 15).unwrap(),
            description: "売掛金の回収".to_string(),
        });

        // When: JSON にシリアライズ
        let json = serde_json::to_string(&event).unwrap();

        // Then: タグ付き形式で保存される
        assert!(json.contains("\"type\":\"Created\""));
        assert!(json.contains("\"journal_id\":\"J-001\""));
    }

    #[test]
    fn test_entry_added_event() {
        // Given: 仕訳明細追加イベント
        let event = JournalEvent::EntryAdded(EntryAddedData {
            entry_id: 1,
            account_code: "1010".to_string(),
            debit_credit: "D".to_string(),
            amount: dec!(100000),
            description: "現金".to_string(),
        });

        // When: JSON にシリアライズ
        let json = serde_json::to_value(&event).unwrap();

        // Then: 正しく保存される
        assert_eq!(json["type"], "EntryAdded");
        assert_eq!(json["data"]["entry_id"], 1);
        assert_eq!(json["data"]["account_code"], "1010");
    }

    #[test]
    fn test_journal_approved_event() {
        // Given: 仕訳承認イベント
        let now = Utc::now();
        let event = JournalEvent::Approved(JournalApprovedData {
            approved_by: "manager@example.com".to_string(),
            approved_at: now,
        });

        // When: JSON にシリアライズ
        let json = serde_json::to_value(&event).unwrap();

        // Then: 正しく保存される
        assert_eq!(json["type"], "Approved");
        assert_eq!(json["data"]["approved_by"], "manager@example.com");
    }

    #[test]
    fn test_journal_event_deserialization() {
        // Given: JSON 形式のイベントデータ
        let json = r#"{
            "type": "Created",
            "data": {
                "journal_id": "J-002",
                "journal_date": "2024-02-01",
                "description": "Test journal"
            }
        }"#;

        // When: デシリアライズ
        let event: JournalEvent = serde_json::from_str(json).unwrap();

        // Then: 正しくパースされる
        match event {
            JournalEvent::Created(data) => {
                assert_eq!(data.journal_id, "J-002");
                assert_eq!(data.description, "Test journal");
            }
            _ => panic!("Expected Created event"),
        }
    }
}
