use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 売上データの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct Sales {
    #[sqlx(rename = "売上番号")]
    pub sales_no: String,
    #[sqlx(rename = "売上日")]
    pub sales_date: Option<NaiveDateTime>,
    #[sqlx(rename = "顧客コード")]
    pub cust_code: String,
    #[sqlx(rename = "顧客枝番")]
    pub cust_sub_no: Option<i32>,
    #[sqlx(rename = "社員コード")]
    pub emp_code: String,
    #[sqlx(rename = "部門コード")]
    pub dept_code: String,
    #[sqlx(rename = "開始日")]
    pub start_date: NaiveDateTime,
    #[sqlx(rename = "倉庫コード")]
    pub wh_code: String,
    #[sqlx(rename = "受注番号")]
    pub order_no: Option<String>,
    #[sqlx(rename = "明細行数")]
    pub detail_count: Option<i32>,
    #[sqlx(rename = "売上金額")]
    pub sales_amount: Option<i32>,
    #[sqlx(rename = "消費税額")]
    pub cmp_tax: i32,
    #[sqlx(rename = "伝票備考")]
    pub slip_comment: Option<String>,
    #[sqlx(rename = "作成日時")]
    pub create_date: NaiveDateTime,
    #[sqlx(rename = "作成者名")]
    pub creator: Option<String>,
    #[sqlx(rename = "更新日時")]
    pub update_date: NaiveDateTime,
    #[sqlx(rename = "更新者名")]
    pub updater: Option<String>,
}

/// 売上データ明細の構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct SalesDetail {
    #[sqlx(rename = "売上番号")]
    pub sales_no: String,
    #[sqlx(rename = "明細番号")]
    pub detail_no: i32,
    #[sqlx(rename = "商品コード")]
    pub prod_code: String,
    #[sqlx(rename = "商品名")]
    pub prod_name: Option<String>,
    #[sqlx(rename = "商品名略称")]
    pub prod_abbr_name: Option<String>,
    #[sqlx(rename = "色")]
    pub color: Option<String>,
    #[sqlx(rename = "サイズ")]
    pub size: Option<String>,
    #[sqlx(rename = "数量")]
    pub quantity: i32,
    #[sqlx(rename = "単位")]
    pub unit: Option<String>,
    #[sqlx(rename = "単価")]
    pub unit_price: Option<i32>,
    #[sqlx(rename = "売上原価")]
    pub prod_cost: Option<i32>,
    #[sqlx(rename = "受注番号")]
    pub order_no: Option<String>,
    #[sqlx(rename = "受注明細番号")]
    pub order_detail_no: Option<i32>,
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

    fn create_test_sales() -> Sales {
        Sales {
            sales_no: "SAL0000001".to_string(),
            sales_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 15).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            ),
            cust_code: "COMP001".to_string(),
            cust_sub_no: Some(1),
            emp_code: "EMP001".to_string(),
            dept_code: "D001".to_string(),
            start_date: NaiveDate::from_ymd_opt(2021, 1, 15).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            wh_code: "WH001".to_string(),
            order_no: Some("ORD0000001".to_string()),
            detail_count: Some(2),
            sales_amount: Some(20000),
            cmp_tax: 2000,
            slip_comment: Some("テスト売上".to_string()),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    fn create_test_sales_detail() -> SalesDetail {
        SalesDetail {
            sales_no: "SAL0000001".to_string(),
            detail_no: 1,
            prod_code: "PROD001".to_string(),
            prod_name: Some("テスト商品".to_string()),
            prod_abbr_name: Some("テスト".to_string()),
            color: Some("赤".to_string()),
            size: Some("M".to_string()),
            quantity: 10,
            unit: Some("個".to_string()),
            unit_price: Some(1000),
            prod_cost: Some(600),
            order_no: Some("ORD0000001".to_string()),
            order_detail_no: Some(1),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[test]
    fn test_sales_creation() {
        let sales = create_test_sales();
        assert_eq!(sales.sales_no, "SAL0000001");
        assert_eq!(sales.cust_code, "COMP001");
        assert_eq!(sales.sales_amount, Some(20000));
    }

    #[test]
    fn test_sales_detail_creation() {
        let detail = create_test_sales_detail();
        assert_eq!(detail.sales_no, "SAL0000001");
        assert_eq!(detail.detail_no, 1);
        assert_eq!(detail.prod_code, "PROD001");
        assert_eq!(detail.quantity, 10);
    }

    #[test]
    fn test_sales_serialize_deserialize() {
        let sales = create_test_sales();
        let json = serde_json::to_string(&sales).expect("Failed to serialize");
        let deserialized: Sales = serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(sales, deserialized);
    }

    #[test]
    fn test_sales_detail_serialize_deserialize() {
        let detail = create_test_sales_detail();
        let json = serde_json::to_string(&detail).expect("Failed to serialize");
        let deserialized: SalesDetail = serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(detail, deserialized);
    }
}
