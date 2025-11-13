use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 商品マスタの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct Product {
    #[sqlx(rename = "商品コード")]
    pub prod_code: String,
    #[sqlx(rename = "商品正式名")]
    pub fullname: String,
    #[sqlx(rename = "商品略称")]
    pub name: String,
    #[sqlx(rename = "商品名カナ")]
    pub kana: String,
    #[sqlx(rename = "商品区分")]
    pub prod_type: Option<String>,
    #[sqlx(rename = "製品型番")]
    pub serial_no: Option<String>,
    #[sqlx(rename = "販売単価")]
    pub unitprice: i32,
    #[sqlx(rename = "仕入単価")]
    pub po_price: Option<i32>,
    #[sqlx(rename = "売上原価")]
    pub prime_cost: i32,
    #[sqlx(rename = "税区分")]
    pub tax_type: i32,
    #[sqlx(rename = "商品分類コード")]
    pub category_code: Option<String>,
    #[sqlx(rename = "雑区分")]
    pub wide_use_type: Option<i32>,
    #[sqlx(rename = "在庫管理対象区分")]
    pub stock_manage_type: Option<i32>,
    #[sqlx(rename = "在庫引当区分")]
    pub stock_reserve_type: Option<i32>,
    #[sqlx(rename = "仕入先コード")]
    pub sup_code: String,
    #[sqlx(rename = "仕入先枝番")]
    pub sup_sub_no: Option<i32>,
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

    fn create_test_product() -> Product {
        Product {
            prod_code: "PROD001".to_string(),
            fullname: "テスト商品フルネーム".to_string(),
            name: "テスト商品".to_string(),
            kana: "テストショウヒン".to_string(),
            prod_type: Some("1".to_string()),
            serial_no: Some("SN-001".to_string()),
            unitprice: 1000,
            po_price: Some(800),
            prime_cost: 750,
            tax_type: 1,
            category_code: Some("CAT001".to_string()),
            wide_use_type: Some(0),
            stock_manage_type: Some(1),
            stock_reserve_type: Some(0),
            sup_code: "SUP001".to_string(),
            sup_sub_no: Some(1),
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
        }
    }

    #[test]
    fn test_product_creation() {
        let product = create_test_product();
        assert_eq!(product.prod_code, "PROD001");
        assert_eq!(product.fullname, "テスト商品フルネーム");
        assert_eq!(product.name, "テスト商品");
        assert_eq!(product.kana, "テストショウヒン");
        assert_eq!(product.unitprice, 1000);
        assert_eq!(product.po_price, Some(800));
        assert_eq!(product.prime_cost, 750);
        assert_eq!(product.tax_type, 1);
    }

    #[test]
    fn test_product_clone() {
        let prod1 = create_test_product();
        let prod2 = prod1.clone();

        assert_eq!(prod1, prod2);
        assert_eq!(prod1.prod_code, prod2.prod_code);
        assert_eq!(prod1.name, prod2.name);
    }

    #[test]
    fn test_product_partial_eq() {
        let prod1 = create_test_product();
        let prod2 = create_test_product();
        let mut prod3 = create_test_product();
        prod3.prod_code = "PROD002".to_string();

        assert_eq!(prod1, prod2);
        assert_ne!(prod1, prod3);
    }

    #[test]
    fn test_product_serialize_deserialize() {
        let product = create_test_product();

        // Serialize to JSON
        let json = serde_json::to_string(&product).expect("Failed to serialize");
        assert!(!json.is_empty());

        // Deserialize from JSON
        let deserialized: Product = serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(product, deserialized);
    }

    #[test]
    fn test_product_optional_fields() {
        let product = Product {
            prod_code: "PROD002".to_string(),
            fullname: "商品2".to_string(),
            name: "商品2".to_string(),
            kana: "ショウヒン2".to_string(),
            prod_type: None,
            serial_no: None,
            unitprice: 500,
            po_price: None,
            prime_cost: 400,
            tax_type: 1,
            category_code: None,
            wide_use_type: None,
            stock_manage_type: None,
            stock_reserve_type: None,
            sup_code: "SUP001".to_string(),
            sup_sub_no: None,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            creator: None,
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            updater: None,
        };

        assert_eq!(product.prod_type, None);
        assert_eq!(product.serial_no, None);
        assert_eq!(product.po_price, None);
        assert_eq!(product.category_code, None);
    }

    #[test]
    fn test_product_price_relationship() {
        let product = create_test_product();
        assert!(product.prime_cost <= product.po_price.unwrap());
        assert!(product.po_price.unwrap() < product.unitprice);
    }
}
