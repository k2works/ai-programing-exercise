use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 部門マスタの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct Department {
    #[sqlx(rename = "部門コード")]
    pub dept_code: String,
    #[sqlx(rename = "開始日")]
    pub start_date: NaiveDateTime,
    #[sqlx(rename = "終了日")]
    pub end_date: Option<NaiveDateTime>,
    #[sqlx(rename = "部門名")]
    pub name: Option<String>,
    #[sqlx(rename = "組織階層")]
    pub layer: i32,
    #[sqlx(rename = "部門パス")]
    pub path: String,
    #[sqlx(rename = "最下層区分")]
    pub lowest_type: i32,
    #[sqlx(rename = "伝票入力可否")]
    pub slip_yn: i32,
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

    fn create_test_department() -> Department {
        Department {
            dept_code: "D001".to_string(),
            start_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            end_date: Some(
                NaiveDate::from_ymd_opt(2100, 12, 31).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            ),
            name: Some("営業部".to_string()),
            layer: 1,
            path: "D001".to_string(),
            lowest_type: 0,
            slip_yn: 1,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[test]
    fn test_department_creation() {
        let dept = create_test_department();
        assert_eq!(dept.dept_code, "D001");
        assert_eq!(dept.name, Some("営業部".to_string()));
        assert_eq!(dept.layer, 1);
        assert_eq!(dept.lowest_type, 0);
        assert_eq!(dept.slip_yn, 1);
    }

    #[test]
    fn test_department_clone() {
        let dept1 = create_test_department();
        let dept2 = dept1.clone();

        assert_eq!(dept1, dept2);
        assert_eq!(dept1.dept_code, dept2.dept_code);
        assert_eq!(dept1.name, dept2.name);
    }

    #[test]
    fn test_department_partial_eq() {
        let dept1 = create_test_department();
        let dept2 = create_test_department();
        let mut dept3 = create_test_department();
        dept3.dept_code = "D002".to_string();

        assert_eq!(dept1, dept2);
        assert_ne!(dept1, dept3);
    }

    #[test]
    fn test_department_serialize_deserialize() {
        let dept = create_test_department();

        // Serialize to JSON
        let json = serde_json::to_string(&dept).expect("Failed to serialize");
        assert!(!json.is_empty());

        // Deserialize from JSON
        let deserialized: Department = serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(dept, deserialized);
    }

    #[test]
    fn test_department_optional_fields() {
        let dept = Department {
            dept_code: "D002".to_string(),
            start_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            end_date: None,
            name: None,
            layer: 1,
            path: "D002".to_string(),
            lowest_type: 0,
            slip_yn: 0,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: None,
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: None,
        };

        assert_eq!(dept.end_date, None);
        assert_eq!(dept.name, None);
        assert_eq!(dept.creator, None);
        assert_eq!(dept.updater, None);
    }
}
