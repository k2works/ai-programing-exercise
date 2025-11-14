use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 顧客マスタの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct Customer {
    #[sqlx(rename = "顧客コード")]
    pub cust_code: String,
    #[sqlx(rename = "顧客枝番")]
    pub cust_sub_no: i32,
    #[sqlx(rename = "顧客区分")]
    pub cust_type: Option<i32>,
    #[sqlx(rename = "請求先コード")]
    pub ar_code: String,
    #[sqlx(rename = "請求先枝番")]
    pub ar_sub_no: Option<i32>,
    #[sqlx(rename = "回収先コード")]
    pub payer_code: String,
    #[sqlx(rename = "回収先枝番")]
    pub payer_sub_no: Option<i32>,
    #[sqlx(rename = "顧客名")]
    pub name: String,
    #[sqlx(rename = "顧客名カナ")]
    pub kana: Option<String>,
    #[sqlx(rename = "自社担当者コード")]
    pub emp_code: String,
    #[sqlx(rename = "顧客締日１")]
    pub cust_close_date1: i32,
    #[sqlx(rename = "顧客支払月１")]
    pub cust_pay_months1: Option<i32>,
    #[sqlx(rename = "顧客支払日１")]
    pub cust_pay_dates1: Option<i32>,
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

    fn create_test_customer() -> Customer {
        Customer {
            cust_code: "COMP001".to_string(),
            cust_sub_no: 1,
            cust_type: Some(1),
            ar_code: "COMP001".to_string(),
            ar_sub_no: Some(1),
            payer_code: "COMP001".to_string(),
            payer_sub_no: Some(1),
            name: "テスト株式会社".to_string(),
            kana: Some("テストカブシキガイシャ".to_string()),
            emp_code: "EMP001".to_string(),
            cust_close_date1: 31,
            cust_pay_months1: Some(1),
            cust_pay_dates1: Some(31),
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
    fn test_customer_creation() {
        let customer = create_test_customer();
        assert_eq!(customer.cust_code, "COMP001");
        assert_eq!(customer.cust_sub_no, 1);
        assert_eq!(customer.name, "テスト株式会社");
    }

    #[test]
    fn test_customer_clone() {
        let cust1 = create_test_customer();
        let cust2 = cust1.clone();

        assert_eq!(cust1, cust2);
        assert_eq!(cust1.cust_code, cust2.cust_code);
    }

    #[test]
    fn test_customer_serialize_deserialize() {
        let customer = create_test_customer();

        let json = serde_json::to_string(&customer).expect("Failed to serialize");
        assert!(!json.is_empty());

        let deserialized: Customer = serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(customer, deserialized);
    }
}
