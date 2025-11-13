use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 顧客別販売単価の構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct PriceByCustomer {
    #[sqlx(rename = "商品コード")]
    pub prod_code: String,
    #[sqlx(rename = "取引先コード")]
    pub comp_code: String,
    #[sqlx(rename = "販売単価")]
    pub unitprice: i32,
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

    fn create_test_price_by_customer() -> PriceByCustomer {
        PriceByCustomer {
            prod_code: "PROD001".to_string(),
            comp_code: "COMP001".to_string(),
            unitprice: 950,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[test]
    fn test_price_by_customer_creation() {
        let price = create_test_price_by_customer();
        assert_eq!(price.prod_code, "PROD001");
        assert_eq!(price.comp_code, "COMP001");
        assert_eq!(price.unitprice, 950);
    }

    #[test]
    fn test_price_by_customer_clone() {
        let price1 = create_test_price_by_customer();
        let price2 = price1.clone();

        assert_eq!(price1, price2);
        assert_eq!(price1.prod_code, price2.prod_code);
        assert_eq!(price1.comp_code, price2.comp_code);
        assert_eq!(price1.unitprice, price2.unitprice);
    }

    #[test]
    fn test_price_by_customer_partial_eq() {
        let price1 = create_test_price_by_customer();
        let price2 = create_test_price_by_customer();
        let mut price3 = create_test_price_by_customer();
        price3.comp_code = "COMP002".to_string();

        assert_eq!(price1, price2);
        assert_ne!(price1, price3);
    }

    #[test]
    fn test_price_by_customer_serialize_deserialize() {
        let price = create_test_price_by_customer();

        // Serialize to JSON
        let json = serde_json::to_string(&price).expect("Failed to serialize");
        assert!(!json.is_empty());

        // Deserialize from JSON
        let deserialized: PriceByCustomer =
            serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(price, deserialized);
    }

    #[test]
    fn test_price_by_customer_optional_fields() {
        let price = PriceByCustomer {
            prod_code: "PROD002".to_string(),
            comp_code: "COMP002".to_string(),
            unitprice: 800,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: None,
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: None,
        };

        assert_eq!(price.creator, None);
        assert_eq!(price.updater, None);
    }
}
