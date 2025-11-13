use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 代替商品の構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct AlternateProduct {
    #[sqlx(rename = "商品コード")]
    pub prod_code: String,
    #[sqlx(rename = "代替商品コード")]
    pub alt_prod_code: String,
    #[sqlx(rename = "優先順位")]
    pub priority: Option<i32>,
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

    fn create_test_alternate_product() -> AlternateProduct {
        AlternateProduct {
            prod_code: "PROD001".to_string(),
            alt_prod_code: "PROD002".to_string(),
            priority: Some(1),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[test]
    fn test_alternate_product_creation() {
        let alt = create_test_alternate_product();
        assert_eq!(alt.prod_code, "PROD001");
        assert_eq!(alt.alt_prod_code, "PROD002");
        assert_eq!(alt.priority, Some(1));
    }

    #[test]
    fn test_alternate_product_clone() {
        let alt1 = create_test_alternate_product();
        let alt2 = alt1.clone();

        assert_eq!(alt1, alt2);
        assert_eq!(alt1.prod_code, alt2.prod_code);
        assert_eq!(alt1.alt_prod_code, alt2.alt_prod_code);
        assert_eq!(alt1.priority, alt2.priority);
    }

    #[test]
    fn test_alternate_product_partial_eq() {
        let alt1 = create_test_alternate_product();
        let alt2 = create_test_alternate_product();
        let mut alt3 = create_test_alternate_product();
        alt3.alt_prod_code = "PROD003".to_string();

        assert_eq!(alt1, alt2);
        assert_ne!(alt1, alt3);
    }

    #[test]
    fn test_alternate_product_serialize_deserialize() {
        let alt = create_test_alternate_product();

        // Serialize to JSON
        let json = serde_json::to_string(&alt).expect("Failed to serialize");
        assert!(!json.is_empty());

        // Deserialize from JSON
        let deserialized: AlternateProduct =
            serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(alt, deserialized);
    }

    #[test]
    fn test_alternate_product_optional_fields() {
        let alt = AlternateProduct {
            prod_code: "PROD001".to_string(),
            alt_prod_code: "PROD003".to_string(),
            priority: None,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: None,
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: None,
        };

        assert_eq!(alt.priority, None);
        assert_eq!(alt.creator, None);
        assert_eq!(alt.updater, None);
    }

    #[test]
    fn test_alternate_product_priority_order() {
        let alt1 = AlternateProduct {
            prod_code: "PROD001".to_string(),
            alt_prod_code: "PROD002".to_string(),
            priority: Some(1),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: None,
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: None,
        };

        let alt2 = AlternateProduct {
            prod_code: "PROD001".to_string(),
            alt_prod_code: "PROD003".to_string(),
            priority: Some(2),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: None,
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: None,
        };

        assert!(alt1.priority.unwrap() < alt2.priority.unwrap());
    }
}
