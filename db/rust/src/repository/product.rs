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
        sqlx::query_as!(
            ProductResponse,
            r#"
            INSERT INTO "商品マスタ" (
                "商品コード", "商品分類コード", "商品正式名", "商品略称", "商品名カナ",
                "商品区分", "製品型番", "販売単価", "仕入単価", "売上原価", "税区分",
                "在庫管理対象区分", "在庫引当区分", "仕入先コード", "仕入先枝番",
                "作成日時", "更新日時"
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17)
            RETURNING
                "商品コード" as prod_code,
                "商品分類コード" as prod_category_code,
                "商品正式名" as fullname,
                "商品略称" as name,
                "商品名カナ" as kana,
                "商品区分" as prod_class,
                "製品型番" as model_number,
                "販売単価" as unitprice,
                "仕入単価" as purchase_price,
                "売上原価" as prime_cost,
                "税区分" as tax_class,
                "雑区分" as misc_class,
                "在庫管理対象区分" as stock_managed,
                "在庫引当区分" as stock_reserve,
                "仕入先コード" as sup_code,
                "仕入先枝番" as sup_seq_num,
                "作成日時" as created_at,
                "更新日時" as updated_at
            "#,
            request.prod_code,
            request.prod_category_code,
            request.fullname,
            request.name,
            request.kana,
            request.prod_class,
            request.model_number,
            request.unitprice,
            request.purchase_price,
            request.prime_cost,
            request.tax_class.unwrap_or(1),
            request.stock_managed,
            request.stock_reserve,
            request.sup_code,
            request.sup_seq_num,
            created_at,
            updated_at
        )
        .fetch_one(pool)
        .await
    }

    /// すべての商品を取得（DTO用）
    pub async fn find_all_as_dto(pool: &PgPool) -> Result<Vec<ProductResponse>, sqlx::Error> {
        sqlx::query_as!(
            ProductResponse,
            r#"
            SELECT
                "商品コード" as prod_code,
                "商品分類コード" as prod_category_code,
                "商品正式名" as fullname,
                "商品略称" as name,
                "商品名カナ" as kana,
                "商品区分" as prod_class,
                "製品型番" as model_number,
                "販売単価" as unitprice,
                "仕入単価" as purchase_price,
                "売上原価" as prime_cost,
                "税区分" as tax_class,
                "雑区分" as misc_class,
                "在庫管理対象区分" as stock_managed,
                "在庫引当区分" as stock_reserve,
                "仕入先コード" as sup_code,
                "仕入先枝番" as sup_seq_num,
                "作成日時" as created_at,
                "更新日時" as updated_at
            FROM "商品マスタ"
            ORDER BY "商品コード"
            "#
        )
        .fetch_all(pool)
        .await
    }

    /// IDで商品を取得（DTO用）
    pub async fn find_by_code_as_dto(
        pool: &PgPool,
        prod_code: &str,
    ) -> Result<Option<ProductResponse>, sqlx::Error> {
        sqlx::query_as!(
            ProductResponse,
            r#"
            SELECT
                "商品コード" as prod_code,
                "商品分類コード" as prod_category_code,
                "商品正式名" as fullname,
                "商品略称" as name,
                "商品名カナ" as kana,
                "商品区分" as prod_class,
                "製品型番" as model_number,
                "販売単価" as unitprice,
                "仕入単価" as purchase_price,
                "売上原価" as prime_cost,
                "税区分" as tax_class,
                "雑区分" as misc_class,
                "在庫管理対象区分" as stock_managed,
                "在庫引当区分" as stock_reserve,
                "仕入先コード" as sup_code,
                "仕入先枝番" as sup_seq_num,
                "作成日時" as created_at,
                "更新日時" as updated_at
            FROM "商品マスタ"
            WHERE "商品コード" = $1
            "#,
            prod_code
        )
        .fetch_optional(pool)
        .await
    }

    /// 商品を更新（DTO用）
    pub async fn update_as_dto(
        pool: &PgPool,
        prod_code: &str,
        request: &UpdateProductRequest,
        updated_at: chrono::NaiveDateTime,
    ) -> Result<ProductResponse, sqlx::Error> {
        sqlx::query_as!(
            ProductResponse,
            r#"
            UPDATE "商品マスタ" SET
                "商品分類コード" = COALESCE($2, "商品分類コード"),
                "商品正式名" = COALESCE($3, "商品正式名"),
                "商品略称" = COALESCE($4, "商品略称"),
                "商品名カナ" = COALESCE($5, "商品名カナ"),
                "商品区分" = COALESCE($6, "商品区分"),
                "製品型番" = COALESCE($7, "製品型番"),
                "販売単価" = COALESCE($8, "販売単価"),
                "仕入単価" = COALESCE($9, "仕入単価"),
                "売上原価" = COALESCE($10, "売上原価"),
                "税区分" = COALESCE($11, "税区分"),
                "在庫管理対象区分" = COALESCE($12, "在庫管理対象区分"),
                "在庫引当区分" = COALESCE($13, "在庫引当区分"),
                "仕入先コード" = COALESCE($14, "仕入先コード"),
                "仕入先枝番" = COALESCE($15, "仕入先枝番"),
                "更新日時" = $16
            WHERE "商品コード" = $1
            RETURNING
                "商品コード" as prod_code,
                "商品分類コード" as prod_category_code,
                "商品正式名" as fullname,
                "商品略称" as name,
                "商品名カナ" as kana,
                "商品区分" as prod_class,
                "製品型番" as model_number,
                "販売単価" as unitprice,
                "仕入単価" as purchase_price,
                "売上原価" as prime_cost,
                "税区分" as tax_class,
                "雑区分" as misc_class,
                "在庫管理対象区分" as stock_managed,
                "在庫引当区分" as stock_reserve,
                "仕入先コード" as sup_code,
                "仕入先枝番" as sup_seq_num,
                "作成日時" as created_at,
                "更新日時" as updated_at
            "#,
            prod_code,
            request.prod_category_code,
            request.fullname,
            request.name,
            request.kana,
            request.prod_class,
            request.model_number,
            request.unitprice,
            request.purchase_price,
            request.prime_cost,
            request.tax_class,
            request.stock_managed,
            request.stock_reserve,
            request.sup_code,
            request.sup_seq_num,
            updated_at
        )
        .fetch_one(pool)
        .await
    }

    /// 商品を削除（商品コード指定）
    pub async fn delete_by_code(pool: &PgPool, prod_code: &str) -> Result<(), sqlx::Error> {
        sqlx::query!(r#"DELETE FROM "商品マスタ" WHERE "商品コード" = $1"#, prod_code)
            .execute(pool)
            .await?;
        Ok(())
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
