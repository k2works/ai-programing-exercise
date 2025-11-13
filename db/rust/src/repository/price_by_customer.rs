use crate::entity::PriceByCustomer;
use sqlx::PgPool;

pub struct PriceByCustomerRepository;

impl PriceByCustomerRepository {
    /// 顧客別販売単価を登録
    pub async fn create(pool: &PgPool, price: &PriceByCustomer) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "顧客別販売単価" (
                "商品コード", "取引先コード", "販売単価",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7)
            "#,
        )
        .bind(&price.prod_code)
        .bind(&price.comp_code)
        .bind(price.unitprice)
        .bind(price.create_date)
        .bind(&price.creator)
        .bind(price.update_date)
        .bind(&price.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 顧客別販売単価を取得
    pub async fn find(
        pool: &PgPool,
        prod_code: &str,
        comp_code: &str,
    ) -> Result<PriceByCustomer, sqlx::Error> {
        sqlx::query_as::<_, PriceByCustomer>(
            r#"SELECT * FROM "顧客別販売単価" WHERE "商品コード" = $1 AND "取引先コード" = $2"#,
        )
        .bind(prod_code)
        .bind(comp_code)
        .fetch_one(pool)
        .await
    }

    /// 顧客別販売単価を更新
    pub async fn update(pool: &PgPool, price: &PriceByCustomer) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "顧客別販売単価" SET
                "販売単価" = $3,
                "更新日時" = $4,
                "更新者名" = $5
            WHERE "商品コード" = $1 AND "取引先コード" = $2
            "#,
        )
        .bind(&price.prod_code)
        .bind(&price.comp_code)
        .bind(price.unitprice)
        .bind(price.update_date)
        .bind(&price.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 顧客別販売単価を削除
    pub async fn delete(
        pool: &PgPool,
        prod_code: &str,
        comp_code: &str,
    ) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"DELETE FROM "顧客別販売単価" WHERE "商品コード" = $1 AND "取引先コード" = $2"#,
        )
        .bind(prod_code)
        .bind(comp_code)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 全件削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "顧客別販売単価""#)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 商品コードで検索
    pub async fn find_by_product(
        pool: &PgPool,
        prod_code: &str,
    ) -> Result<Vec<PriceByCustomer>, sqlx::Error> {
        sqlx::query_as::<_, PriceByCustomer>(
            r#"SELECT * FROM "顧客別販売単価" WHERE "商品コード" = $1"#,
        )
        .bind(prod_code)
        .fetch_all(pool)
        .await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::{Product, ProductCategory};
    use crate::repository::{ProductCategoryRepository, ProductRepository};
    use crate::test_support::with_test_pool;
    use chrono::NaiveDate;

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

    fn create_test_product() -> Product {
        Product {
            prod_code: "PROD001".to_string(),
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

    fn create_test_price() -> PriceByCustomer {
        PriceByCustomer {
            prod_code: "PROD001".to_string(),
            comp_code: "COMP001".to_string(),
            unitprice: 950,
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
        let category = create_test_category();
        let product = create_test_product();
        let price = create_test_price();
        with_test_pool(|pool| async move {
            // クリーンアップ
            PriceByCustomerRepository::delete_all(&pool).await.ok();
            ProductRepository::delete_all(&pool).await.ok();
            ProductCategoryRepository::delete_all(&pool).await.ok();

            // 商品分類と商品を先に登録
            ProductCategoryRepository::create(&pool, &category)
                .await
                .expect("Failed to create category");
            ProductRepository::create(&pool, &product)
                .await
                .expect("Failed to create product");

            // 顧客別販売単価を登録
            PriceByCustomerRepository::create(&pool, &price)
                .await
                .expect("Failed to create price");

            // 取得
            let result = PriceByCustomerRepository::find(&pool, &price.prod_code, &price.comp_code)
                .await
                .expect("Failed to find price");

            assert_eq!(result.prod_code, price.prod_code);
            assert_eq!(result.comp_code, price.comp_code);
            assert_eq!(result.unitprice, price.unitprice);
        })
        .await;
    }

    #[tokio::test]
    async fn test_update() {
        let category = create_test_category();
        let product = create_test_product();
        let mut price = create_test_price();
        with_test_pool(|pool| async move {
            // クリーンアップ
            PriceByCustomerRepository::delete_all(&pool).await.ok();
            ProductRepository::delete_all(&pool).await.ok();
            ProductCategoryRepository::delete_all(&pool).await.ok();

            // 商品分類と商品を先に登録
            ProductCategoryRepository::create(&pool, &category)
                .await
                .expect("Failed to create category");
            ProductRepository::create(&pool, &product)
                .await
                .expect("Failed to create product");

            // 顧客別販売単価を登録
            PriceByCustomerRepository::create(&pool, &price)
                .await
                .expect("Failed to create price");

            // 更新
            price.unitprice = 900;
            PriceByCustomerRepository::update(&pool, &price)
                .await
                .expect("Failed to update price");

            // 取得して確認
            let result = PriceByCustomerRepository::find(&pool, &price.prod_code, &price.comp_code)
                .await
                .expect("Failed to find price");

            assert_eq!(result.unitprice, 900);
        })
        .await;
    }

    #[tokio::test]
    async fn test_delete() {
        let category = create_test_category();
        let product = create_test_product();
        let price = create_test_price();
        with_test_pool(|pool| async move {
            // クリーンアップ
            PriceByCustomerRepository::delete_all(&pool).await.ok();
            ProductRepository::delete_all(&pool).await.ok();
            ProductCategoryRepository::delete_all(&pool).await.ok();

            // 商品分類と商品を先に登録
            ProductCategoryRepository::create(&pool, &category)
                .await
                .expect("Failed to create category");
            ProductRepository::create(&pool, &product)
                .await
                .expect("Failed to create product");

            // 顧客別販売単価を登録
            PriceByCustomerRepository::create(&pool, &price)
                .await
                .expect("Failed to create price");

            // 削除
            PriceByCustomerRepository::delete(&pool, &price.prod_code, &price.comp_code)
                .await
                .expect("Failed to delete price");

            // 取得できないことを確認
            let result =
                PriceByCustomerRepository::find(&pool, &price.prod_code, &price.comp_code).await;
            assert!(result.is_err());
        })
        .await;
    }

    #[tokio::test]
    async fn test_find_by_product() {
        let category = create_test_category();
        let product = create_test_product();
        let price1 = create_test_price();
        let mut price2 = create_test_price();
        price2.comp_code = "COMP002".to_string();
        with_test_pool(|pool| async move {
            // クリーンアップ
            PriceByCustomerRepository::delete_all(&pool).await.ok();
            ProductRepository::delete_all(&pool).await.ok();
            ProductCategoryRepository::delete_all(&pool).await.ok();

            // 商品分類と商品を先に登録
            ProductCategoryRepository::create(&pool, &category)
                .await
                .expect("Failed to create category");
            ProductRepository::create(&pool, &product)
                .await
                .expect("Failed to create product");

            // 顧客別販売単価を登録
            PriceByCustomerRepository::create(&pool, &price1)
                .await
                .expect("Failed to create price1");
            PriceByCustomerRepository::create(&pool, &price2)
                .await
                .expect("Failed to create price2");

            // 商品コードで検索
            let results = PriceByCustomerRepository::find_by_product(&pool, "PROD001")
                .await
                .expect("Failed to find by product");

            assert_eq!(results.len(), 2);
        })
        .await;
    }
}
