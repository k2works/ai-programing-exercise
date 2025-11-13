use chrono::NaiveDateTime;
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// 社員マスタの構造体
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize, FromRow)]
pub struct Employee {
    #[sqlx(rename = "社員コード")]
    pub emp_code: String,
    #[sqlx(rename = "社員名")]
    pub name: Option<String>,
    #[sqlx(rename = "社員名カナ")]
    pub kana: Option<String>,
    #[sqlx(rename = "パスワード")]
    pub login_password: Option<String>,
    #[sqlx(rename = "電話番号")]
    pub tel: Option<String>,
    #[sqlx(rename = "FAX番号")]
    pub fax: Option<String>,
    #[sqlx(rename = "部門コード")]
    pub dept_code: String,
    #[sqlx(rename = "開始日")]
    pub start_date: NaiveDateTime,
    #[sqlx(rename = "職種コード")]
    pub occu_code: String,
    #[sqlx(rename = "承認権限コード")]
    pub approval_code: String,
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

    fn create_test_employee() -> Employee {
        Employee {
            emp_code: "E001".to_string(),
            name: Some("田中 太郎".to_string()),
            kana: Some("タナカ タロウ".to_string()),
            login_password: Some("password".to_string()),
            tel: Some("090-1234-5678".to_string()),
            fax: Some("03-1234-5678".to_string()),
            dept_code: "D001".to_string(),
            start_date: NaiveDate::from_ymd_opt(2021, 4, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            occu_code: "01".to_string(),
            approval_code: "02".to_string(),
            create_date: NaiveDate::from_ymd_opt(2021, 4, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 4, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[test]
    fn test_employee_creation() {
        let emp = create_test_employee();
        assert_eq!(emp.emp_code, "E001");
        assert_eq!(emp.name, Some("田中 太郎".to_string()));
        assert_eq!(emp.kana, Some("タナカ タロウ".to_string()));
        assert_eq!(emp.dept_code, "D001");
        assert_eq!(emp.occu_code, "01");
        assert_eq!(emp.approval_code, "02");
    }

    #[test]
    fn test_employee_clone() {
        let emp1 = create_test_employee();
        let emp2 = emp1.clone();

        assert_eq!(emp1, emp2);
        assert_eq!(emp1.emp_code, emp2.emp_code);
        assert_eq!(emp1.name, emp2.name);
        assert_eq!(emp1.dept_code, emp2.dept_code);
    }

    #[test]
    fn test_employee_partial_eq() {
        let emp1 = create_test_employee();
        let emp2 = create_test_employee();
        let mut emp3 = create_test_employee();
        emp3.emp_code = "E002".to_string();

        assert_eq!(emp1, emp2);
        assert_ne!(emp1, emp3);
    }

    #[test]
    fn test_employee_serialize_deserialize() {
        let emp = create_test_employee();

        // Serialize to JSON
        let json = serde_json::to_string(&emp).expect("Failed to serialize");
        assert!(!json.is_empty());

        // Deserialize from JSON
        let deserialized: Employee = serde_json::from_str(&json).expect("Failed to deserialize");
        assert_eq!(emp, deserialized);
    }

    #[test]
    fn test_employee_optional_fields() {
        let emp = Employee {
            emp_code: "E002".to_string(),
            name: None,
            kana: None,
            login_password: None,
            tel: None,
            fax: None,
            dept_code: "D001".to_string(),
            start_date: NaiveDate::from_ymd_opt(2021, 4, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            occu_code: "01".to_string(),
            approval_code: "02".to_string(),
            create_date: NaiveDate::from_ymd_opt(2021, 4, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            creator: None,
            update_date: NaiveDate::from_ymd_opt(2021, 4, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            updater: None,
        };

        assert_eq!(emp.name, None);
        assert_eq!(emp.kana, None);
        assert_eq!(emp.login_password, None);
        assert_eq!(emp.tel, None);
        assert_eq!(emp.fax, None);
        assert_eq!(emp.creator, None);
        assert_eq!(emp.updater, None);
    }

    #[test]
    fn test_employee_department_relationship() {
        let emp = create_test_employee();
        // 社員は部門に所属する
        assert_eq!(emp.dept_code, "D001");
        assert!(!emp.dept_code.is_empty());
    }
}
