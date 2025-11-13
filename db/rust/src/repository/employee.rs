use crate::entity::Employee;
use sqlx::PgPool;

pub struct EmployeeRepository;

impl EmployeeRepository {
    /// 社員を登録
    pub async fn create(pool: &PgPool, emp: &Employee) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "社員マスタ" (
                "社員コード", "社員名", "社員名カナ", "パスワード", "電話番号", "FAX番号",
                "部門コード", "開始日", "職種コード", "承認権限コード",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)
            "#,
        )
        .bind(&emp.emp_code)
        .bind(&emp.name)
        .bind(&emp.kana)
        .bind(&emp.login_password)
        .bind(&emp.tel)
        .bind(&emp.fax)
        .bind(&emp.dept_code)
        .bind(emp.start_date)
        .bind(&emp.occu_code)
        .bind(&emp.approval_code)
        .bind(emp.create_date)
        .bind(&emp.creator)
        .bind(emp.update_date)
        .bind(&emp.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 社員を取得
    pub async fn find_by_code(pool: &PgPool, emp_code: &str) -> Result<Employee, sqlx::Error> {
        sqlx::query_as::<_, Employee>(r#"SELECT * FROM "社員マスタ" WHERE "社員コード" = $1"#)
            .bind(emp_code)
            .fetch_one(pool)
            .await
    }

    /// 全件削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "社員マスタ""#).execute(pool).await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::Employee;
    use crate::test_support::with_test_pool;
    use chrono::NaiveDate;

    /// テスト用の社員データを作成
    fn create_test_employee() -> Employee {
        Employee {
            emp_code: "EMP999".to_string(),
            name: Some("伊藤 裕子".to_string()),
            kana: Some("イトウ ユウコ".to_string()),
            login_password: Some("password".to_string()),
            tel: Some("090-1234-5678".to_string()),
            fax: Some("03-1234-5678".to_string()),
            dept_code: "11101".to_string(),
            start_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            occu_code: "".to_string(),
            approval_code: "".to_string(),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[tokio::test]
    async fn test_employee_create() {
        let emp = create_test_employee();
        with_test_pool(|pool| async move {
            // テーブルをクリーンアップ
            EmployeeRepository::delete_all(&pool).await.expect("Failed to cleanup");

            // 1. 社員を登録
            EmployeeRepository::create(&pool, &emp).await.expect("Failed to insert employee");

            // 2. 登録されたデータを取得して検証
            let result = EmployeeRepository::find_by_code(&pool, &emp.emp_code)
                .await
                .expect("Failed to fetch employee");

            assert_eq!(result.emp_code, emp.emp_code);
            assert_eq!(result.name, emp.name);
            assert_eq!(result.dept_code, emp.dept_code);
        })
        .await;
    }
}
