use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 受注データの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct Order {
    #[sqlx(rename = "受注番号")]
    pub order_no: String,
    #[sqlx(rename = "受注日")]
    pub order_date: Option<NaiveDateTime>,
    #[sqlx(rename = "納品予定日")]
    pub delivery_date: Option<NaiveDateTime>,
    #[sqlx(rename = "顧客コード")]
    pub cust_code: String,
    #[sqlx(rename = "顧客枝番")]
    pub cust_sub_no: Option<i32>,
    #[sqlx(rename = "敬称区分")]
    pub title_type: Option<i32>,
    #[sqlx(rename = "郵便番号")]
    pub zip_code: Option<String>,
    #[sqlx(rename = "住所１")]
    pub address1: Option<String>,
    #[sqlx(rename = "住所２")]
    pub address2: Option<String>,
    #[sqlx(rename = "社員コード")]
    pub emp_code: String,
    #[sqlx(rename = "部門コード")]
    pub dept_code: String,
    #[sqlx(rename = "明細行数")]
    pub detail_count: Option<i32>,
    #[sqlx(rename = "受注金額")]
    pub order_amount: Option<i32>,
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

/// 受注データ明細の構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct OrderDetail {
    #[sqlx(rename = "受注番号")]
    pub order_no: String,
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

    fn create_test_order() -> Order {
        Order {
            order_no: "ORD0000001".to_string(),
            order_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 10)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
            ),
            delivery_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 20)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
            ),
            cust_code: "COMP001".to_string(),
            cust_sub_no: Some(1),
            title_type: Some(1),
            zip_code: Some("100-0001".to_string()),
            address1: Some("東京都千代田区".to_string()),
            address2: Some("1-1-1".to_string()),
            emp_code: "EMP001".to_string(),
            dept_code: "D001".to_string(),
            detail_count: Some(2),
            order_amount: Some(20000),
            cmp_tax: 2000,
            slip_comment: Some("テスト受注".to_string()),
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

    fn create_test_order_detail() -> OrderDetail {
        OrderDetail {
            order_no: "ORD0000001".to_string(),
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
    fn test_order_creation() {
        let order = create_test_order();
        assert_eq!(order.order_no, "ORD0000001");
        assert_eq!(order.cust_code, "COMP001");
        assert_eq!(order.order_amount, Some(20000));
    }

    #[test]
    fn test_order_detail_creation() {
        let detail = create_test_order_detail();
        assert_eq!(detail.order_no, "ORD0000001");
        assert_eq!(detail.detail_no, 1);
        assert_eq!(detail.prod_code, "PROD001");
        assert_eq!(detail.quantity, 10);
    }

    #[test]
    fn test_order_serialize_deserialize() {
        let order = create_test_order();
        let json = serde_json::to_string(&order).expect("Failed to serialize");
        let deserialized: Order = serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(order, deserialized);
    }

    #[test]
    fn test_order_detail_serialize_deserialize() {
        let detail = create_test_order_detail();
        let json = serde_json::to_string(&detail).expect("Failed to serialize");
        let deserialized: OrderDetail =
            serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(detail, deserialized);
    }
}
