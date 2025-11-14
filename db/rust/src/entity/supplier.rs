use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 仕入先マスタの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct Supplier {
    #[sqlx(rename = "仕入先コード")]
    pub sup_code: String,
    #[sqlx(rename = "仕入先枝番")]
    pub sup_sub_no: i32,
    #[sqlx(rename = "仕入先名")]
    pub name: String,
    #[sqlx(rename = "仕入先名カナ")]
    pub kana: Option<String>,
    #[sqlx(rename = "仕入先締日")]
    pub sup_close_date: i32,
    #[sqlx(rename = "仕入先支払月")]
    pub sup_pay_months: Option<i32>,
    #[sqlx(rename = "仕入先支払日")]
    pub sup_pay_dates: Option<i32>,
    #[sqlx(rename = "仕入先支払方法")]
    pub pay_method_type: Option<i32>,
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

    fn create_test_supplier() -> Supplier {
        Supplier {
            sup_code: "COMP001".to_string(),
            sup_sub_no: 1,
            name: "テスト株式会社".to_string(),
            kana: Some("テストカブシキガイシャ".to_string()),
            sup_close_date: 31,
            sup_pay_months: Some(1),
            sup_pay_dates: Some(31),
            pay_method_type: Some(1),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[test]
    fn test_supplier_creation() {
        let supplier = create_test_supplier();
        assert_eq!(supplier.sup_code, "COMP001");
        assert_eq!(supplier.sup_sub_no, 1);
        assert_eq!(supplier.name, "テスト株式会社");
    }

    #[test]
    fn test_supplier_clone() {
        let sup1 = create_test_supplier();
        let sup2 = sup1.clone();

        assert_eq!(sup1, sup2);
        assert_eq!(sup1.sup_code, sup2.sup_code);
    }

    #[test]
    fn test_supplier_serialize_deserialize() {
        let supplier = create_test_supplier();

        let json = serde_json::to_string(&supplier).expect("Failed to serialize");
        assert!(!json.is_empty());

        let deserialized: Supplier = serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(supplier, deserialized);
    }
}
