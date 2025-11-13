use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 商品分類マスタの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct ProductCategory {
    #[sqlx(rename = "商品分類コード")]
    pub category_code: String,
    #[sqlx(rename = "商品分類名")]
    pub name: Option<String>,
    #[sqlx(rename = "商品分類階層")]
    pub layer: i32,
    #[sqlx(rename = "商品分類パス")]
    pub path: Option<String>,
    #[sqlx(rename = "最下層区分")]
    pub lowest_type: Option<i32>,
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

    fn create_test_product_category() -> ProductCategory {
        ProductCategory {
            category_code: "CAT001".to_string(),
            name: Some("電子部品".to_string()),
            layer: 1,
            path: Some("/CAT001".to_string()),
            lowest_type: Some(1),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[test]
    fn test_product_category_creation() {
        let category = create_test_product_category();
        assert_eq!(category.category_code, "CAT001");
        assert_eq!(category.name, Some("電子部品".to_string()));
        assert_eq!(category.layer, 1);
        assert_eq!(category.path, Some("/CAT001".to_string()));
        assert_eq!(category.lowest_type, Some(1));
    }

    #[test]
    fn test_product_category_clone() {
        let cat1 = create_test_product_category();
        let cat2 = cat1.clone();

        assert_eq!(cat1, cat2);
        assert_eq!(cat1.category_code, cat2.category_code);
        assert_eq!(cat1.name, cat2.name);
    }

    #[test]
    fn test_product_category_partial_eq() {
        let cat1 = create_test_product_category();
        let cat2 = create_test_product_category();
        let mut cat3 = create_test_product_category();
        cat3.category_code = "CAT002".to_string();

        assert_eq!(cat1, cat2);
        assert_ne!(cat1, cat3);
    }

    #[test]
    fn test_product_category_serialize_deserialize() {
        let category = create_test_product_category();

        // Serialize to JSON
        let json = serde_json::to_string(&category).expect("Failed to serialize");
        assert!(!json.is_empty());

        // Deserialize from JSON
        let deserialized: ProductCategory =
            serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(category, deserialized);
    }

    #[test]
    fn test_product_category_optional_fields() {
        let category = ProductCategory {
            category_code: "CAT002".to_string(),
            name: None,
            layer: 1,
            path: None,
            lowest_type: None,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: None,
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: None,
        };

        assert_eq!(category.name, None);
        assert_eq!(category.path, None);
        assert_eq!(category.lowest_type, None);
        assert_eq!(category.creator, None);
        assert_eq!(category.updater, None);
    }
}
