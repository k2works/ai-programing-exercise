use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 発注データの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct PurchaseOrder {
    #[sqlx(rename = "発注番号")]
    pub po_no: String,
    #[sqlx(rename = "発注日")]
    pub po_date: Option<NaiveDateTime>,
    #[sqlx(rename = "仕入先コード")]
    pub sup_code: String,
    #[sqlx(rename = "仕入先枝番")]
    pub sup_sub_no: Option<i32>,
    #[sqlx(rename = "発注担当者コード")]
    pub emp_code: String,
    #[sqlx(rename = "指定納期")]
    pub due_date: Option<NaiveDateTime>,
    #[sqlx(rename = "倉庫コード")]
    pub wh_code: String,
    #[sqlx(rename = "発注金額合計")]
    pub po_amount: Option<i32>,
    #[sqlx(rename = "消費税合計")]
    pub cmp_tax: i32,
    #[sqlx(rename = "備考")]
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

/// 発注データ明細の構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct PurchaseOrderDetail {
    #[sqlx(rename = "発注番号")]
    pub po_no: String,
    #[sqlx(rename = "発注行番号")]
    pub po_row_no: i32,
    #[sqlx(rename = "商品コード")]
    pub prod_code: String,
    #[sqlx(rename = "商品名")]
    pub prod_name: Option<String>,
    #[sqlx(rename = "発注数量")]
    pub quantity: i32,
    #[sqlx(rename = "単価")]
    pub unit_price: Option<i32>,
    #[sqlx(rename = "入荷予定数量")]
    pub expected_qty: Option<i32>,
    #[sqlx(rename = "入荷済数量")]
    pub received_qty: Option<i32>,
    #[sqlx(rename = "完了フラグ")]
    pub complete_flg: i32,
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
    fn test_purchase_order_creation() {
        let po = PurchaseOrder {
            po_no: "PO0000001".to_string(),
            po_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 5).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            ),
            sup_code: "COMP001".to_string(),
            sup_sub_no: Some(1),
            emp_code: "EMP999".to_string(),
            due_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 15).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            ),
            wh_code: "WH1".to_string(),
            po_amount: Some(50000),
            cmp_tax: 5000,
            slip_comment: Some("テスト発注".to_string()),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        };

        assert_eq!(po.po_no, "PO0000001");
        assert_eq!(po.sup_code, "COMP001");
        assert_eq!(po.po_amount, Some(50000));
    }

    #[test]
    fn test_purchase_order_detail_creation() {
        let detail = PurchaseOrderDetail {
            po_no: "PO0000001".to_string(),
            po_row_no: 1,
            prod_code: "PROD001".to_string(),
            prod_name: Some("テスト商品".to_string()),
            quantity: 100,
            unit_price: Some(500),
            expected_qty: Some(100),
            received_qty: Some(0),
            complete_flg: 0,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        };

        assert_eq!(detail.po_no, "PO0000001");
        assert_eq!(detail.prod_code, "PROD001");
        assert_eq!(detail.quantity, 100);
        assert_eq!(detail.complete_flg, 0);
    }

    #[test]
    fn test_purchase_order_serialize_deserialize() {
        let po = PurchaseOrder {
            po_no: "PO0000001".to_string(),
            po_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 5).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            ),
            sup_code: "COMP001".to_string(),
            sup_sub_no: Some(1),
            emp_code: "EMP999".to_string(),
            due_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 15).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            ),
            wh_code: "WH1".to_string(),
            po_amount: Some(50000),
            cmp_tax: 5000,
            slip_comment: Some("テスト発注".to_string()),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        };

        let json = serde_json::to_string(&po).unwrap();
        let deserialized: PurchaseOrder = serde_json::from_str(&json).unwrap();
        assert_eq!(po, deserialized);
    }
}
