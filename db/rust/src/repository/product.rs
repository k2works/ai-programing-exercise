use crate::dto::product::{CreateProductRequest, ProductResponse, UpdateProductRequest};
use crate::entity::Product;
use sqlx::PgPool;

pub struct ProductRepository;

impl ProductRepository {
    /// 商品を登録
    pub async fn create(pool: &PgPool, product: &Product) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "商品マスタ" (
                "商品コード", "商品正式名", "商品略称", "商品名カナ", "商品区分", "製品型番",
                "販売単価", "仕入単価", "売上原価", "税区分", "商品分類コード",
                "雑区分", "在庫管理対象区分", "在庫引当区分", "仕入先コード", "仕入先枝番",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19, $20)
            "#,
        )
        .bind(&product.prod_code)
        .bind(&product.fullname)
        .bind(&product.name)
        .bind(&product.kana)
        .bind(&product.prod_type)
        .bind(&product.serial_no)
        .bind(product.unitprice)
        .bind(product.po_price)
        .bind(product.prime_cost)
        .bind(product.tax_type)
        .bind(&product.category_code)
        .bind(product.wide_use_type)
        .bind(product.stock_manage_type)
        .bind(product.stock_reserve_type)
        .bind(&product.sup_code)
        .bind(product.sup_sub_no)
        .bind(product.create_date)
        .bind(&product.creator)
        .bind(product.update_date)
        .bind(&product.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 商品を取得
    pub async fn find_by_code(pool: &PgPool, prod_code: &str) -> Result<Product, sqlx::Error> {
        sqlx::query_as::<_, Product>(r#"SELECT * FROM "商品マスタ" WHERE "商品コード" = $1"#)
            .bind(prod_code)
            .fetch_one(pool)
            .await
    }

    /// 商品を更新
    pub async fn update(pool: &PgPool, product: &Product) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "商品マスタ" SET
                "商品正式名" = $2,
                "商品略称" = $3,
                "商品名カナ" = $4,
                "商品区分" = $5,
                "製品型番" = $6,
                "販売単価" = $7,
                "仕入単価" = $8,
                "売上原価" = $9,
                "税区分" = $10,
                "商品分類コード" = $11,
                "雑区分" = $12,
                "在庫管理対象区分" = $13,
                "在庫引当区分" = $14,
                "仕入先コード" = $15,
                "仕入先枝番" = $16,
                "更新日時" = $17,
                "更新者名" = $18
            WHERE "商品コード" = $1
            "#,
        )
        .bind(&product.prod_code)
        .bind(&product.fullname)
        .bind(&product.name)
        .bind(&product.kana)
        .bind(&product.prod_type)
        .bind(&product.serial_no)
        .bind(product.unitprice)
        .bind(product.po_price)
        .bind(product.prime_cost)
        .bind(product.tax_type)
        .bind(&product.category_code)
        .bind(product.wide_use_type)
        .bind(product.stock_manage_type)
        .bind(product.stock_reserve_type)
        .bind(&product.sup_code)
        .bind(product.sup_sub_no)
        .bind(product.update_date)
        .bind(&product.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 商品を削除
    pub async fn delete(pool: &PgPool, prod_code: &str) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "商品マスタ" WHERE "商品コード" = $1"#)
            .bind(prod_code)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 全件削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "商品マスタ""#).execute(pool).await?;
        Ok(())
    }

    /// 件数を取得
    pub async fn count(pool: &PgPool) -> Result<i64, sqlx::Error> {
        let (count,): (i64,) =
            sqlx::query_as(r#"SELECT COUNT(*) FROM "商品マスタ""#).fetch_one(pool).await?;
        Ok(count)
    }

    /// 商品分類で検索
    pub async fn find_by_category(
        pool: &PgPool,
        category_code: &str,
    ) -> Result<Vec<Product>, sqlx::Error> {
        sqlx::query_as::<_, Product>(r#"SELECT * FROM "商品マスタ" WHERE "商品分類コード" = $1"#)
            .bind(category_code)
            .fetch_all(pool)
            .await
    }

    // ========================================
    // DTO用のメソッド（API層で使用）
    // ========================================

    /// 商品を作成（DTO用）
    pub async fn create_as_dto(
        pool: &PgPool,
        request: &CreateProductRequest,
        created_at: chrono::NaiveDateTime,
        updated_at: chrono::NaiveDateTime,
    ) -> Result<ProductResponse, sqlx::Error> {
        // Entity を作成
        let entity = Product {
            prod_code: request.prod_code.clone(),
            fullname: request.fullname.clone(),
            name: request.name.clone(),
            kana: request.kana.clone(),
            prod_type: request.prod_class.clone(),
            serial_no: request.model_number.clone(),
            unitprice: request.unitprice,
            po_price: request.purchase_price,
            prime_cost: request.prime_cost,
            tax_type: request.tax_class.unwrap_or(1),
            category_code: request.prod_category_code.clone(),
            wide_use_type: None, // misc_class は DB で自動設定
            stock_manage_type: request.stock_managed,
            stock_reserve_type: request.stock_reserve,
            sup_code: request.sup_code.clone(),
            sup_sub_no: request.sup_seq_num,
            create_date: created_at,
            creator: None,
            update_date: updated_at,
            updater: None,
        };

        // Repository の create メソッドで Entity を保存
        Self::create(pool, &entity).await?;

        // 保存された Entity を取得
        let saved_entity = Self::find_by_code(pool, &entity.prod_code).await?;

        // Entity を DTO に変換
        Ok(ProductResponse::from_entity(saved_entity))
    }

    /// すべての商品を取得（DTO用）
    pub async fn find_all_as_dto(pool: &PgPool) -> Result<Vec<ProductResponse>, sqlx::Error> {
        // Entity を全件取得
        let entities =
            sqlx::query_as::<_, Product>(r#"SELECT * FROM "商品マスタ" ORDER BY "商品コード""#)
                .fetch_all(pool)
                .await?;

        // Entity を DTO に変換
        Ok(entities.into_iter().map(ProductResponse::from_entity).collect())
    }

    /// IDで商品を取得（DTO用）
    pub async fn find_by_code_as_dto(
        pool: &PgPool,
        prod_code: &str,
    ) -> Result<Option<ProductResponse>, sqlx::Error> {
        // Repository の find_by_code メソッドで Entity を取得
        match Self::find_by_code(pool, prod_code).await {
            Ok(entity) => Ok(Some(ProductResponse::from_entity(entity))),
            Err(sqlx::Error::RowNotFound) => Ok(None),
            Err(e) => Err(e),
        }
    }

    /// 商品を更新（DTO用）
    pub async fn update_as_dto(
        pool: &PgPool,
        prod_code: &str,
        request: &UpdateProductRequest,
        updated_at: chrono::NaiveDateTime,
    ) -> Result<ProductResponse, sqlx::Error> {
        // 既存の Entity を取得
        let mut entity = Self::find_by_code(pool, prod_code).await?;

        // UpdateProductRequest のフィールドで Entity を更新
        if let Some(ref category_code) = request.prod_category_code {
            entity.category_code = Some(category_code.clone());
        }
        if let Some(ref fullname) = request.fullname {
            entity.fullname = fullname.clone();
        }
        if let Some(ref name) = request.name {
            entity.name = name.clone();
        }
        if let Some(ref kana) = request.kana {
            entity.kana = kana.clone();
        }
        if let Some(ref prod_class) = request.prod_class {
            entity.prod_type = Some(prod_class.clone());
        }
        if let Some(ref model_number) = request.model_number {
            entity.serial_no = Some(model_number.clone());
        }
        if let Some(unitprice) = request.unitprice {
            entity.unitprice = unitprice;
        }
        if let Some(purchase_price) = request.purchase_price {
            entity.po_price = Some(purchase_price);
        }
        if let Some(prime_cost) = request.prime_cost {
            entity.prime_cost = prime_cost;
        }
        if let Some(tax_class) = request.tax_class {
            entity.tax_type = tax_class;
        }
        if let Some(stock_managed) = request.stock_managed {
            entity.stock_manage_type = Some(stock_managed);
        }
        if let Some(stock_reserve) = request.stock_reserve {
            entity.stock_reserve_type = Some(stock_reserve);
        }
        if let Some(ref sup_code) = request.sup_code {
            entity.sup_code = sup_code.clone();
        }
        if let Some(sup_seq_num) = request.sup_seq_num {
            entity.sup_sub_no = Some(sup_seq_num);
        }
        entity.update_date = updated_at;

        // Repository の update メソッドで Entity を更新
        Self::update(pool, &entity).await?;

        // 更新された Entity を取得
        let updated_entity = Self::find_by_code(pool, prod_code).await?;

        // Entity を DTO に変換
        Ok(ProductResponse::from_entity(updated_entity))
    }

    /// 商品を削除（商品コード指定、DTO 用）
    pub async fn delete_by_code(pool: &PgPool, prod_code: &str) -> Result<(), sqlx::Error> {
        // Repository の delete メソッドを使用
        Self::delete(pool, prod_code).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::entity::ProductCategory;
    use crate::repository::ProductCategoryRepository;
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
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[tokio::test]
    async fn test_create_and_find() {
        let category = create_test_category();
        let product = create_test_product();
        with_test_pool(|pool| async move {
            // クリーンアップ
            ProductRepository::delete_all(&pool).await.ok();
            ProductCategoryRepository::delete_all(&pool).await.ok();

            // 商品分類を先に登録
            ProductCategoryRepository::create(&pool, &category)
                .await
                .expect("Failed to create category");

            // 商品を登録
            ProductRepository::create(&pool, &product).await.expect("Failed to create product");

            // 取得
            let result = ProductRepository::find_by_code(&pool, &product.prod_code)
                .await
                .expect("Failed to find product");

            assert_eq!(result.prod_code, product.prod_code);
            assert_eq!(result.name, product.name);
            assert_eq!(result.unitprice, product.unitprice);
        })
        .await;
    }

    #[tokio::test]
    async fn test_update() {
        let category = create_test_category();
        let mut product = create_test_product();
        with_test_pool(|pool| async move {
            // クリーンアップ
            ProductRepository::delete_all(&pool).await.ok();
            ProductCategoryRepository::delete_all(&pool).await.ok();

            // 商品分類を先に登録
            ProductCategoryRepository::create(&pool, &category)
                .await
                .expect("Failed to create category");

            // 商品を登録
            ProductRepository::create(&pool, &product).await.expect("Failed to create product");

            // 更新
            product.unitprice = 1200;
            ProductRepository::update(&pool, &product).await.expect("Failed to update product");

            // 取得して確認
            let result = ProductRepository::find_by_code(&pool, &product.prod_code)
                .await
                .expect("Failed to find product");

            assert_eq!(result.unitprice, 1200);
        })
        .await;
    }

    #[tokio::test]
    async fn test_delete() {
        let category = create_test_category();
        let product = create_test_product();
        with_test_pool(|pool| async move {
            // クリーンアップ
            ProductRepository::delete_all(&pool).await.ok();
            ProductCategoryRepository::delete_all(&pool).await.ok();

            // 商品分類を先に登録
            ProductCategoryRepository::create(&pool, &category)
                .await
                .expect("Failed to create category");

            // 商品を登録
            ProductRepository::create(&pool, &product).await.expect("Failed to create product");

            // 削除
            ProductRepository::delete(&pool, &product.prod_code)
                .await
                .expect("Failed to delete product");

            // 取得できないことを確認
            let result = ProductRepository::find_by_code(&pool, &product.prod_code).await;
            assert!(result.is_err());
        })
        .await;
    }

    #[tokio::test]
    async fn test_find_by_category() {
        let category = create_test_category();
        let product1 = create_test_product();
        let mut product2 = create_test_product();
        product2.prod_code = "PROD002".to_string();
        with_test_pool(|pool| async move {
            // クリーンアップ
            ProductRepository::delete_all(&pool).await.ok();
            ProductCategoryRepository::delete_all(&pool).await.ok();

            // 商品分類を先に登録
            ProductCategoryRepository::create(&pool, &category)
                .await
                .expect("Failed to create category");

            // 商品を登録
            ProductRepository::create(&pool, &product1).await.expect("Failed to create product1");
            ProductRepository::create(&pool, &product2).await.expect("Failed to create product2");

            // カテゴリで検索
            let results = ProductRepository::find_by_category(&pool, "CAT001")
                .await
                .expect("Failed to find by category");

            assert_eq!(results.len(), 2);
        })
        .await;
    }
}
