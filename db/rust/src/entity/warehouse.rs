use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 倉庫マスタの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct Warehouse {
    #[sqlx(rename = "倉庫コード")]
    pub wh_code: String,
    #[sqlx(rename = "倉庫名")]
    pub wh_name: Option<String>,
    #[sqlx(rename = "倉庫名略称")]
    pub wh_abbr_name: Option<String>,
    #[sqlx(rename = "郵便番号")]
    pub zip_code: Option<String>,
    #[sqlx(rename = "都道府県")]
    pub state: Option<String>,
    #[sqlx(rename = "住所１")]
    pub address1: Option<String>,
    #[sqlx(rename = "住所２")]
    pub address2: Option<String>,
    #[sqlx(rename = "電話番号")]
    pub tel: Option<String>,
    #[sqlx(rename = "ＦＡＸ番号")]
    pub fax: Option<String>,
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
    fn test_warehouse_creation() {
        let warehouse = Warehouse {
            wh_code: "WH1".to_string(),
            wh_name: Some("東京倉庫".to_string()),
            wh_abbr_name: Some("東京".to_string()),
            zip_code: Some("100-0001".to_string()),
            state: Some("東京都".to_string()),
            address1: Some("千代田区".to_string()),
            address2: Some("1-1-1".to_string()),
            tel: Some("03-1234-5678".to_string()),
            fax: Some("03-1234-5679".to_string()),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            updater: Some("admin".to_string()),
        };

        assert_eq!(warehouse.wh_code, "WH1");
        assert_eq!(warehouse.wh_name, Some("東京倉庫".to_string()));
    }

    #[test]
    fn test_warehouse_clone() {
        let warehouse = Warehouse {
            wh_code: "WH1".to_string(),
            wh_name: Some("東京倉庫".to_string()),
            wh_abbr_name: Some("東京".to_string()),
            zip_code: Some("100-0001".to_string()),
            state: Some("東京都".to_string()),
            address1: Some("千代田区".to_string()),
            address2: Some("1-1-1".to_string()),
            tel: Some("03-1234-5678".to_string()),
            fax: Some("03-1234-5679".to_string()),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            updater: Some("admin".to_string()),
        };

        let cloned = warehouse.clone();
        assert_eq!(warehouse, cloned);
    }

    #[test]
    fn test_warehouse_serialize_deserialize() {
        let warehouse = Warehouse {
            wh_code: "WH1".to_string(),
            wh_name: Some("東京倉庫".to_string()),
            wh_abbr_name: Some("東京".to_string()),
            zip_code: Some("100-0001".to_string()),
            state: Some("東京都".to_string()),
            address1: Some("千代田区".to_string()),
            address2: Some("1-1-1".to_string()),
            tel: Some("03-1234-5678".to_string()),
            fax: Some("03-1234-5679".to_string()),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            updater: Some("admin".to_string()),
        };

        let json = serde_json::to_string(&warehouse).unwrap();
        let deserialized: Warehouse = serde_json::from_str(&json).unwrap();
        assert_eq!(warehouse, deserialized);
    }
}
