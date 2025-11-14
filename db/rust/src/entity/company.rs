use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 取引先マスタの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct Company {
    #[sqlx(rename = "取引先コード")]
    pub comp_code: String,
    #[sqlx(rename = "取引先名")]
    pub name: String,
    #[sqlx(rename = "取引先名カナ")]
    pub kana: Option<String>,
    #[sqlx(rename = "仕入先区分")]
    pub sup_type: Option<i32>,
    #[sqlx(rename = "郵便番号")]
    pub zip_code: Option<String>,
    #[sqlx(rename = "都道府県")]
    pub state: Option<String>,
    #[sqlx(rename = "住所１")]
    pub address1: Option<String>,
    #[sqlx(rename = "住所２")]
    pub address2: Option<String>,
    #[sqlx(rename = "取引禁止フラグ")]
    pub no_sales_flg: Option<i32>,
    #[sqlx(rename = "雑区分")]
    pub wide_use_type: Option<i32>,
    #[sqlx(rename = "取引先グループコード")]
    pub comp_group_code: String,
    #[sqlx(rename = "与信限度額")]
    pub max_credit: Option<i32>,
    #[sqlx(rename = "与信一時増加枠")]
    pub temp_credit_up: Option<i32>,
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

    fn create_test_company() -> Company {
        Company {
            comp_code: "COMP001".to_string(),
            name: "テスト株式会社".to_string(),
            kana: Some("テストカブシキガイシャ".to_string()),
            sup_type: Some(1),
            zip_code: Some("100-0001".to_string()),
            state: Some("東京都".to_string()),
            address1: Some("千代田区".to_string()),
            address2: Some("1-1-1".to_string()),
            no_sales_flg: Some(0),
            wide_use_type: Some(0),
            comp_group_code: "GRP001".to_string(),
            max_credit: Some(10000000),
            temp_credit_up: Some(0),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[test]
    fn test_company_creation() {
        let company = create_test_company();
        assert_eq!(company.comp_code, "COMP001");
        assert_eq!(company.name, "テスト株式会社");
        assert_eq!(company.comp_group_code, "GRP001");
    }

    #[test]
    fn test_company_clone() {
        let comp1 = create_test_company();
        let comp2 = comp1.clone();

        assert_eq!(comp1, comp2);
        assert_eq!(comp1.comp_code, comp2.comp_code);
    }

    #[test]
    fn test_company_serialize_deserialize() {
        let company = create_test_company();

        let json = serde_json::to_string(&company).expect("Failed to serialize");
        assert!(!json.is_empty());

        let deserialized: Company = serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(company, deserialized);
    }
}
