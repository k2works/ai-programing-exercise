use crate::entity::Department;
use chrono::NaiveDateTime;
use sqlx::PgPool;

pub struct DepartmentRepository;

impl DepartmentRepository {
    /// 部門を登録
    pub async fn create(pool: &PgPool, dept: &Department) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "部門マスタ" (
                "部門コード", "開始日", "終了日", "部門名", "組織階層",
                "部門パス", "最下層区分", "伝票入力可否",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
            "#,
        )
        .bind(&dept.dept_code)
        .bind(dept.start_date)
        .bind(dept.end_date)
        .bind(&dept.name)
        .bind(dept.layer)
        .bind(&dept.path)
        .bind(dept.lowest_type)
        .bind(dept.slip_yn)
        .bind(dept.create_date)
        .bind(&dept.creator)
        .bind(dept.update_date)
        .bind(&dept.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 部門を取得
    pub async fn find_by_code_and_date(
        pool: &PgPool,
        dept_code: &str,
        start_date: &NaiveDateTime,
    ) -> Result<Department, sqlx::Error> {
        sqlx::query_as::<_, Department>(
            r#"SELECT * FROM "部門マスタ" WHERE "部門コード" = $1 AND "開始日" = $2"#,
        )
        .bind(dept_code)
        .bind(start_date)
        .fetch_one(pool)
        .await
    }

    /// 部門名を更新
    pub async fn update_name(
        pool: &PgPool,
        dept_code: &str,
        start_date: &NaiveDateTime,
        new_name: &str,
    ) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"UPDATE "部門マスタ" SET "部門名" = $1 WHERE "部門コード" = $2 AND "開始日" = $3"#,
        )
        .bind(new_name)
        .bind(dept_code)
        .bind(start_date)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 部門を削除
    pub async fn delete(
        pool: &PgPool,
        dept_code: &str,
        start_date: &NaiveDateTime,
    ) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "部門マスタ" WHERE "部門コード" = $1 AND "開始日" = $2"#)
            .bind(dept_code)
            .bind(start_date)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 全件削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "部門マスタ""#)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 件数を取得
    pub async fn count(pool: &PgPool) -> Result<i64, sqlx::Error> {
        let (count,): (i64,) = sqlx::query_as(r#"SELECT COUNT(*) FROM "部門マスタ""#)
            .fetch_one(pool)
            .await?;
        Ok(count)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::create_pool;
    use crate::entity::Department;
    use chrono::NaiveDate;

    /// テスト用のデータベース接続プールを作成
    async fn setup() -> PgPool {
        create_pool().await.expect("Failed to create pool")
    }

    /// テスト用の部門データを作成
    fn create_test_department() -> Department {
        Department {
            dept_code: "11101".to_string(),
            start_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            end_date: Some(
                NaiveDate::from_ymd_opt(2100, 12, 31)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
            ),
            name: Some("新規部署".to_string()),
            layer: 1,
            path: "D0001".to_string(),
            lowest_type: 1,
            slip_yn: 0,
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

    #[tokio::test]
    async fn test_department_create() {
        let pool = setup().await;
        let dept = create_test_department();

        // テーブルをクリーンアップ
        DepartmentRepository::delete_all(&pool)
            .await
            .expect("Failed to cleanup");

        // 1. 部門を登録
        DepartmentRepository::create(&pool, &dept)
            .await
            .expect("Failed to insert department");

        // 2. 登録されたデータを取得して検証
        let result =
            DepartmentRepository::find_by_code_and_date(&pool, &dept.dept_code, &dept.start_date)
                .await
                .expect("Failed to fetch department");

        assert_eq!(result.dept_code, dept.dept_code);
        assert_eq!(result.name, dept.name);
    }

    #[tokio::test]
    async fn test_department_update() {
        let pool = setup().await;
        let dept = create_test_department();

        // テーブルをクリーンアップ
        DepartmentRepository::delete_all(&pool)
            .await
            .expect("Failed to cleanup");

        // データを登録
        DepartmentRepository::create(&pool, &dept)
            .await
            .expect("Failed to insert department");

        // 1. 部門名を更新
        DepartmentRepository::update_name(&pool, &dept.dept_code, &dept.start_date, "更新部署")
            .await
            .expect("Failed to update department");

        // 2. 更新されたか検証
        let result =
            DepartmentRepository::find_by_code_and_date(&pool, &dept.dept_code, &dept.start_date)
                .await
                .expect("Failed to fetch department");

        assert_eq!(result.name, Some("更新部署".to_string()));
    }

    #[tokio::test]
    async fn test_department_delete() {
        let pool = setup().await;
        let dept = create_test_department();

        // テーブルをクリーンアップ
        DepartmentRepository::delete_all(&pool)
            .await
            .expect("Failed to cleanup");

        // データを登録
        DepartmentRepository::create(&pool, &dept)
            .await
            .expect("Failed to insert department");

        // 1. 部門を削除
        DepartmentRepository::delete(&pool, &dept.dept_code, &dept.start_date)
            .await
            .expect("Failed to delete department");

        // 2. テーブルが空になったか検証
        let count = DepartmentRepository::count(&pool)
            .await
            .expect("Failed to count");

        assert_eq!(count, 0);
    }
}
