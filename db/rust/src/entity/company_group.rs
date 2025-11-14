use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 取引先グループマスタの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct CompanyGroup {
    #[sqlx(rename = "取引先グループコード")]
    pub comp_group_code: String,
    #[sqlx(rename = "取引先グループ名")]
    pub name: Option<String>,
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

    fn create_test_company_group() -> CompanyGroup {
        CompanyGroup {
            comp_group_code: "GRP001".to_string(),
            name: Some("優良企業グループ".to_string()),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[test]
    fn test_company_group_creation() {
        let group = create_test_company_group();
        assert_eq!(group.comp_group_code, "GRP001");
        assert_eq!(group.name, Some("優良企業グループ".to_string()));
    }

    #[test]
    fn test_company_group_clone() {
        let group1 = create_test_company_group();
        let group2 = group1.clone();

        assert_eq!(group1, group2);
        assert_eq!(group1.comp_group_code, group2.comp_group_code);
    }

    #[test]
    fn test_company_group_serialize_deserialize() {
        let group = create_test_company_group();

        let json = serde_json::to_string(&group).expect("Failed to serialize");
        assert!(!json.is_empty());

        let deserialized: CompanyGroup =
            serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(group, deserialized);
    }
}
