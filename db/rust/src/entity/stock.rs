use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 在庫データの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct Stock {
    #[sqlx(rename = "倉庫コード")]
    pub wh_code: String,
    #[sqlx(rename = "商品コード")]
    pub prod_code: String,
    #[sqlx(rename = "ロット番号")]
    pub lot_no: String,
    #[sqlx(rename = "在庫区分")]
    pub stock_type: String,
    #[sqlx(rename = "良品区分")]
    pub quality_type: String,
    #[sqlx(rename = "実在庫数")]
    pub actual: i32,
    #[sqlx(rename = "有効在庫数")]
    pub valid: i32,
    #[sqlx(rename = "最終出荷日")]
    pub last_delivery_date: Option<NaiveDateTime>,
    #[sqlx(rename = "作成日時")]
    pub create_date: NaiveDateTime,
    #[sqlx(rename = "作成者名")]
    pub creator: Option<String>,
    #[sqlx(rename = "更新日時")]
    pub update_date: NaiveDateTime,
    #[sqlx(rename = "更新者名")]
    pub updater: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;
    use chrono::NaiveDate;

    #[test]
    fn test_stock_creation() {
        let stock = Stock {
            wh_code: "WH1".to_string(),
            prod_code: "PROD001".to_string(),
            lot_no: "LOT20210101".to_string(),
            stock_type: "1".to_string(),
            quality_type: "G".to_string(),
            actual: 100,
            valid: 100,
            last_delivery_date: None,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        };

        assert_eq!(stock.wh_code, "WH1");
        assert_eq!(stock.prod_code, "PROD001");
        assert_eq!(stock.actual, 100);
        assert_eq!(stock.valid, 100);
    }

    #[test]
    fn test_stock_clone() {
        let stock = Stock {
            wh_code: "WH1".to_string(),
            prod_code: "PROD001".to_string(),
            lot_no: "LOT20210101".to_string(),
            stock_type: "1".to_string(),
            quality_type: "G".to_string(),
            actual: 100,
            valid: 100,
            last_delivery_date: None,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        };

        let cloned = stock.clone();
        assert_eq!(stock, cloned);
    }

    #[test]
    fn test_stock_serialize_deserialize() {
        let stock = Stock {
            wh_code: "WH1".to_string(),
            prod_code: "PROD001".to_string(),
            lot_no: "LOT20210101".to_string(),
            stock_type: "1".to_string(),
            quality_type: "G".to_string(),
            actual: 100,
            valid: 100,
            last_delivery_date: None,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        };

        let json = serde_json::to_string(&stock).unwrap();
        let deserialized: Stock = serde_json::from_str(&json).unwrap();
        assert_eq!(stock, deserialized);
    }
}
