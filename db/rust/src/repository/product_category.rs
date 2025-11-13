use crate::entity::ProductCategory;
use sqlx::PgPool;

pub struct ProductCategoryRepository;

impl ProductCategoryRepository {
    /// 商品分類を登録
    pub async fn create(pool: &PgPool, category: &ProductCategory) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "商品分類マスタ" (
                "商品分類コード", "商品分類名", "商品分類階層", "商品分類パス", "最下層区分",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9)
            "#,
        )
        .bind(&category.category_code)
        .bind(&category.name)
        .bind(category.layer)
        .bind(&category.path)
        .bind(category.lowest_type)
        .bind(category.create_date)
        .bind(&category.creator)
        .bind(category.update_date)
        .bind(&category.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 商品分類を取得
    pub async fn find_by_code(
        pool: &PgPool,
        category_code: &str,
    ) -> Result<ProductCategory, sqlx::Error> {
        sqlx::query_as::<_, ProductCategory>(
            r#"SELECT * FROM "商品分類マスタ" WHERE "商品分類コード" = $1"#,
        )
        .bind(category_code)
        .fetch_one(pool)
        .await
    }

    /// 商品分類名を更新
    pub async fn update_name(
        pool: &PgPool,
        category_code: &str,
        new_name: &str,
    ) -> Result<(), sqlx::Error> {
        sqlx::query(r#"UPDATE "商品分類マスタ" SET "商品分類名" = $1 WHERE "商品分類コード" = $2"#)
            .bind(new_name)
            .bind(category_code)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 商品分類を削除
    pub async fn delete(pool: &PgPool, category_code: &str) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "商品分類マスタ" WHERE "商品分類コード" = $1"#)
            .bind(category_code)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 全件削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "商品分類マスタ""#).execute(pool).await?;
        Ok(())
    }

    /// 件数を取得
    pub async fn count(pool: &PgPool) -> Result<i64, sqlx::Error> {
        let (count,): (i64,) =
            sqlx::query_as(r#"SELECT COUNT(*) FROM "商品分類マスタ""#).fetch_one(pool).await?;
        Ok(count)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::with_test_pool;
    use chrono::NaiveDate;

    fn create_test_category() -> ProductCategory {
        ProductCategory {
            category_code: "CAT001".to_string(),
            name: Some("電子部品".to_string()),
            layer: 1,
            path: Some("/CAT001".to_string()),
            lowest_type: Some(1),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[tokio::test]
    async fn test_create_and_find() {
        let category = create_test_category();
        with_test_pool(|pool| async move {
            // 外部キー制約のため、先に商品マスタを削除
            sqlx::query(r#"DELETE FROM "商品マスタ""#).execute(&pool).await.ok();
            ProductCategoryRepository::delete_all(&pool).await.expect("Failed to cleanup");

            // 登録
            ProductCategoryRepository::create(&pool, &category)
                .await
                .expect("Failed to create category");

            // 取得
            let result = ProductCategoryRepository::find_by_code(&pool, &category.category_code)
                .await
                .expect("Failed to find category");

            assert_eq!(result.category_code, category.category_code);
            assert_eq!(result.name, category.name);
            assert_eq!(result.layer, category.layer);
        })
        .await;
    }

    #[tokio::test]
    async fn test_update_name() {
        let category = create_test_category();
        with_test_pool(|pool| async move {
            // 外部キー制約のため、先に商品マスタを削除
            sqlx::query(r#"DELETE FROM "商品マスタ""#).execute(&pool).await.ok();
            ProductCategoryRepository::delete_all(&pool).await.expect("Failed to cleanup");

            // 登録
            ProductCategoryRepository::create(&pool, &category)
                .await
                .expect("Failed to create category");

            // 更新
            let new_name = "機械部品";
            ProductCategoryRepository::update_name(&pool, &category.category_code, new_name)
                .await
                .expect("Failed to update name");

            // 取得して確認
            let result = ProductCategoryRepository::find_by_code(&pool, &category.category_code)
                .await
                .expect("Failed to find category");

            assert_eq!(result.name, Some(new_name.to_string()));
        })
        .await;
    }

    #[tokio::test]
    async fn test_delete() {
        let category = create_test_category();
        with_test_pool(|pool| async move {
            // 外部キー制約のため、先に商品マスタを削除
            sqlx::query(r#"DELETE FROM "商品マスタ""#).execute(&pool).await.ok();
            ProductCategoryRepository::delete_all(&pool).await.expect("Failed to cleanup");

            // 登録
            ProductCategoryRepository::create(&pool, &category)
                .await
                .expect("Failed to create category");

            // 削除
            ProductCategoryRepository::delete(&pool, &category.category_code)
                .await
                .expect("Failed to delete category");

            // 取得できないことを確認
            let result =
                ProductCategoryRepository::find_by_code(&pool, &category.category_code).await;
            assert!(result.is_err());
        })
        .await;
    }

    #[tokio::test]
    async fn test_count() {
        let category = create_test_category();
        with_test_pool(|pool| async move {
            // 外部キー制約のため、先に商品マスタを削除
            sqlx::query(r#"DELETE FROM "商品マスタ""#).execute(&pool).await.ok();
            ProductCategoryRepository::delete_all(&pool).await.expect("Failed to cleanup");

            // 初期カウント
            let count = ProductCategoryRepository::count(&pool).await.expect("Failed to count");
            assert_eq!(count, 0);

            // 登録
            ProductCategoryRepository::create(&pool, &category)
                .await
                .expect("Failed to create category");

            // カウント確認
            let count = ProductCategoryRepository::count(&pool).await.expect("Failed to count");
            assert_eq!(count, 1);
        })
        .await;
    }
}
