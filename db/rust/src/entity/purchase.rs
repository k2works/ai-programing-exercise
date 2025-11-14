use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 仕入データの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct Purchase {
    #[sqlx(rename = "仕入番号")]
    pub pu_no: String,
    #[sqlx(rename = "仕入日")]
    pub pu_date: Option<NaiveDateTime>,
    #[sqlx(rename = "仕入先コード")]
    pub sup_code: String,
    #[sqlx(rename = "仕入先枝番")]
    pub sup_sub_no: Option<i32>,
    #[sqlx(rename = "仕入担当者コード")]
    pub emp_code: String,
    #[sqlx(rename = "開始日")]
    pub start_date: NaiveDateTime,
    #[sqlx(rename = "発注番号")]
    pub po_no: Option<String>,
    #[sqlx(rename = "部門コード")]
    pub dept_code: String,
    #[sqlx(rename = "仕入金額合計")]
    pub pu_amount: Option<i32>,
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

/// 仕入データ明細の構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct PurchaseDetail {
    #[sqlx(rename = "仕入番号")]
    pub pu_no: String,
    #[sqlx(rename = "仕入行番号")]
    pub pu_row_no: i32,
    #[sqlx(rename = "商品コード")]
    pub prod_code: String,
    #[sqlx(rename = "商品名")]
    pub prod_name: Option<String>,
    #[sqlx(rename = "仕入数量")]
    pub quantity: i32,
    #[sqlx(rename = "単価")]
    pub unit_price: Option<i32>,
    #[sqlx(rename = "倉庫コード")]
    pub wh_code: String,
    #[sqlx(rename = "ロット番号")]
    pub lot_no: String,
    #[sqlx(rename = "発注番号")]
    pub po_no: Option<String>,
    #[sqlx(rename = "発注行番号")]
    pub po_row_no: Option<i32>,
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
    fn test_purchase_creation() {
        let pu = Purchase {
            pu_no: "PU0000001".to_string(),
            pu_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 10)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
            ),
            sup_code: "COMP001".to_string(),
            sup_sub_no: Some(1),
            emp_code: "EMP999".to_string(),
            start_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            po_no: Some("PO0000001".to_string()),
            dept_code: "D001".to_string(),
            pu_amount: Some(50000),
            cmp_tax: 5000,
            slip_comment: Some("テスト仕入".to_string()),
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

        assert_eq!(pu.pu_no, "PU0000001");
        assert_eq!(pu.sup_code, "COMP001");
        assert_eq!(pu.pu_amount, Some(50000));
        assert_eq!(pu.po_no, Some("PO0000001".to_string()));
    }

    #[test]
    fn test_purchase_detail_creation() {
        let detail = PurchaseDetail {
            pu_no: "PU0000001".to_string(),
            pu_row_no: 1,
            prod_code: "PROD001".to_string(),
            prod_name: Some("テスト商品".to_string()),
            quantity: 100,
            unit_price: Some(500),
            wh_code: "WH1".to_string(),
            lot_no: "LOT20210110".to_string(),
            po_no: Some("PO0000001".to_string()),
            po_row_no: Some(1),
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

        assert_eq!(detail.pu_no, "PU0000001");
        assert_eq!(detail.prod_code, "PROD001");
        assert_eq!(detail.quantity, 100);
        assert_eq!(detail.lot_no, "LOT20210110");
        assert_eq!(detail.po_no, Some("PO0000001".to_string()));
    }

    #[test]
    fn test_purchase_serialize_deserialize() {
        let pu = Purchase {
            pu_no: "PU0000001".to_string(),
            pu_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 10)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
            ),
            sup_code: "COMP001".to_string(),
            sup_sub_no: Some(1),
            emp_code: "EMP999".to_string(),
            start_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            po_no: Some("PO0000001".to_string()),
            dept_code: "D001".to_string(),
            pu_amount: Some(50000),
            cmp_tax: 5000,
            slip_comment: Some("テスト仕入".to_string()),
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

        let json = serde_json::to_string(&pu).unwrap();
        let deserialized: Purchase = serde_json::from_str(&json).unwrap();
        assert_eq!(pu, deserialized);
    }
}
