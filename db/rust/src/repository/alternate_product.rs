use crate::entity::AlternateProduct;
use sqlx::PgPool;

pub struct AlternateProductRepository;

impl AlternateProductRepository {
    /// 代替商品を登録
    pub async fn create(pool: &PgPool, alt: &AlternateProduct) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "代替商品" (
                "商品コード", "代替商品コード", "優先順位",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7)
            "#,
        )
        .bind(&alt.prod_code)
        .bind(&alt.alt_prod_code)
        .bind(alt.priority)
        .bind(alt.create_date)
        .bind(&alt.creator)
        .bind(alt.update_date)
        .bind(&alt.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 代替商品を取得
    pub async fn find(
        pool: &PgPool,
        prod_code: &str,
        alt_prod_code: &str,
    ) -> Result<AlternateProduct, sqlx::Error> {
        sqlx::query_as::<_, AlternateProduct>(
            r#"SELECT * FROM "代替商品" WHERE "商品コード" = $1 AND "代替商品コード" = $2"#,
        )
        .bind(prod_code)
        .bind(alt_prod_code)
        .fetch_one(pool)
        .await
    }

    /// 代替商品を更新
    pub async fn update(pool: &PgPool, alt: &AlternateProduct) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "代替商品" SET
                "優先順位" = $3,
                "更新日時" = $4,
                "更新者名" = $5
            WHERE "商品コード" = $1 AND "代替商品コード" = $2
            "#,
        )
        .bind(&alt.prod_code)
        .bind(&alt.alt_prod_code)
        .bind(alt.priority)
        .bind(alt.update_date)
        .bind(&alt.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 代替商品を削除
    pub async fn delete(
        pool: &PgPool,
        prod_code: &str,
        alt_prod_code: &str,
    ) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "代替商品" WHERE "商品コード" = $1 AND "代替商品コード" = $2"#)
            .bind(prod_code)
            .bind(alt_prod_code)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 全件削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "代替商品""#)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 商品コードで検索（優先順位でソート）
    pub async fn find_by_product(
        pool: &PgPool,
        prod_code: &str,
    ) -> Result<Vec<AlternateProduct>, sqlx::Error> {
        sqlx::query_as::<_, AlternateProduct>(
            r#"SELECT * FROM "代替商品" WHERE "商品コード" = $1 ORDER BY "優先順位""#,
        )
        .bind(prod_code)
        .fetch_all(pool)
        .await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::create_pool;
    use crate::entity::{Product, ProductCategory};
    use crate::repository::{ProductCategoryRepository, ProductRepository};
    use chrono::NaiveDate;

    async fn setup() -> PgPool {
        let pool = create_pool().await.expect("Failed to create pool");
        AlternateProductRepository::delete_all(&pool)
            .await
            .expect("Failed to cleanup");
        ProductRepository::delete_all(&pool)
            .await
            .expect("Failed to cleanup");
        ProductCategoryRepository::delete_all(&pool)
            .await
            .expect("Failed to cleanup");
        pool
    }

    fn create_test_category() -> ProductCategory {
        ProductCategory {
            category_code: "CAT001".to_string(),
            name: Some("電子部品".to_string()),
            layer: 1,
            path: Some("/CAT001".to_string()),
            lowest_type: Some(1),
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

    fn create_test_product(prod_code: &str) -> Product {
        Product {
            prod_code: prod_code.to_string(),
            fullname: "テスト商品フルネーム".to_string(),
            name: "テスト商品".to_string(),
            kana: "テストショウヒン".to_string(),
            prod_type: Some("1".to_string()),
            serial_no: Some("SN-001".to_string()),
            unitprice: 1000,
            po_price: Some(800),
            prime_cost: 750,
            tax_type: 1,
            category_code: Some("CAT001".to_string()),
            wide_use_type: Some(0),
            stock_manage_type: Some(1),
            stock_reserve_type: Some(0),
            sup_code: "SUP001".to_string(),
            sup_sub_no: Some(1),
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

    fn create_test_alternate() -> AlternateProduct {
        AlternateProduct {
            prod_code: "PROD001".to_string(),
            alt_prod_code: "PROD002".to_string(),
            priority: Some(1),
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
    async fn test_create_and_find() {
        let pool = setup().await;
        let category = create_test_category();
        let product1 = create_test_product("PROD001");
        let product2 = create_test_product("PROD002");
        let alt = create_test_alternate();

        // 商品分類と商品を先に登録
        ProductCategoryRepository::create(&pool, &category)
            .await
            .expect("Failed to create category");
        ProductRepository::create(&pool, &product1)
            .await
            .expect("Failed to create product1");
        ProductRepository::create(&pool, &product2)
            .await
            .expect("Failed to create product2");

        // 代替商品を登録
        AlternateProductRepository::create(&pool, &alt)
            .await
            .expect("Failed to create alternate");

        // 取得
        let result = AlternateProductRepository::find(&pool, &alt.prod_code, &alt.alt_prod_code)
            .await
            .expect("Failed to find alternate");

        assert_eq!(result.prod_code, alt.prod_code);
        assert_eq!(result.alt_prod_code, alt.alt_prod_code);
        assert_eq!(result.priority, alt.priority);
    }

    #[tokio::test]
    async fn test_update() {
        let pool = setup().await;
        let category = create_test_category();
        let product1 = create_test_product("PROD001");
        let product2 = create_test_product("PROD002");
        let mut alt = create_test_alternate();

        // 商品分類と商品を先に登録
        ProductCategoryRepository::create(&pool, &category)
            .await
            .expect("Failed to create category");
        ProductRepository::create(&pool, &product1)
            .await
            .expect("Failed to create product1");
        ProductRepository::create(&pool, &product2)
            .await
            .expect("Failed to create product2");

        // 代替商品を登録
        AlternateProductRepository::create(&pool, &alt)
            .await
            .expect("Failed to create alternate");

        // 更新
        alt.priority = Some(2);
        AlternateProductRepository::update(&pool, &alt)
            .await
            .expect("Failed to update alternate");

        // 取得して確認
        let result = AlternateProductRepository::find(&pool, &alt.prod_code, &alt.alt_prod_code)
            .await
            .expect("Failed to find alternate");

        assert_eq!(result.priority, Some(2));
    }

    #[tokio::test]
    async fn test_delete() {
        let pool = setup().await;
        let category = create_test_category();
        let product1 = create_test_product("PROD001");
        let product2 = create_test_product("PROD002");
        let alt = create_test_alternate();

        // 商品分類と商品を先に登録
        ProductCategoryRepository::create(&pool, &category)
            .await
            .expect("Failed to create category");
        ProductRepository::create(&pool, &product1)
            .await
            .expect("Failed to create product1");
        ProductRepository::create(&pool, &product2)
            .await
            .expect("Failed to create product2");

        // 代替商品を登録
        AlternateProductRepository::create(&pool, &alt)
            .await
            .expect("Failed to create alternate");

        // 削除
        AlternateProductRepository::delete(&pool, &alt.prod_code, &alt.alt_prod_code)
            .await
            .expect("Failed to delete alternate");

        // 取得できないことを確認
        let result =
            AlternateProductRepository::find(&pool, &alt.prod_code, &alt.alt_prod_code).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_find_by_product_with_priority() {
        let pool = setup().await;
        let category = create_test_category();
        let product1 = create_test_product("PROD001");
        let product2 = create_test_product("PROD002");
        let product3 = create_test_product("PROD003");

        // 商品分類と商品を先に登録
        ProductCategoryRepository::create(&pool, &category)
            .await
            .expect("Failed to create category");
        ProductRepository::create(&pool, &product1)
            .await
            .expect("Failed to create product1");
        ProductRepository::create(&pool, &product2)
            .await
            .expect("Failed to create product2");
        ProductRepository::create(&pool, &product3)
            .await
            .expect("Failed to create product3");

        // 代替商品を登録（優先順位が異なる）
        let alt1 = AlternateProduct {
            prod_code: "PROD001".to_string(),
            alt_prod_code: "PROD002".to_string(),
            priority: Some(2),
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

        let alt2 = AlternateProduct {
            prod_code: "PROD001".to_string(),
            alt_prod_code: "PROD003".to_string(),
            priority: Some(1),
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

        AlternateProductRepository::create(&pool, &alt1)
            .await
            .expect("Failed to create alt1");
        AlternateProductRepository::create(&pool, &alt2)
            .await
            .expect("Failed to create alt2");

        // 商品コードで検索（優先順位でソート）
        let results = AlternateProductRepository::find_by_product(&pool, "PROD001")
            .await
            .expect("Failed to find by product");

        assert_eq!(results.len(), 2);
        // 優先順位でソートされていることを確認
        assert_eq!(results[0].alt_prod_code, "PROD003"); // priority 1
        assert_eq!(results[1].alt_prod_code, "PROD002"); // priority 2
    }
}
