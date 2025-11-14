//! 商品サービス層
//! ビジネスロジックとトランザクション制御

use crate::dto::product::{CreateProductRequest, ProductResponse, UpdateProductRequest};
use anyhow::Result;
use chrono::Utc;
use sqlx::PgPool;

pub struct ProductService {
    pool: PgPool,
}

impl ProductService {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    /// 商品を作成
    pub async fn create_product(&self, request: CreateProductRequest) -> Result<ProductResponse> {
        // ビジネスルールの検証
        request.validate_business_rules().map_err(|e| anyhow::anyhow!(e))?;

        let now = Utc::now().naive_utc();

        let product = sqlx::query_as!(
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
            now,
            now
        )
        .fetch_one(&self.pool)
        .await?;

        Ok(product)
    }

    /// すべての商品を取得
    pub async fn get_all_products(&self) -> Result<Vec<ProductResponse>> {
        let products = sqlx::query_as!(
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
        .fetch_all(&self.pool)
        .await?;

        Ok(products)
    }

    /// IDで商品を取得
    pub async fn get_product_by_id(&self, prod_code: &str) -> Result<Option<ProductResponse>> {
        let product = sqlx::query_as!(
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
        .fetch_optional(&self.pool)
        .await?;

        Ok(product)
    }

    /// 商品を更新
    pub async fn update_product(
        &self,
        prod_code: &str,
        request: UpdateProductRequest,
    ) -> Result<ProductResponse> {
        // 存在確認
        let existing = self.get_product_by_id(prod_code).await?;
        if existing.is_none() {
            anyhow::bail!("商品が見つかりません");
        }

        let existing = existing.unwrap();

        // ビジネスルールの検証
        let new_unitprice = request.unitprice.unwrap_or(existing.unitprice);
        let new_prime_cost = request.prime_cost.unwrap_or(existing.prime_cost);

        if new_unitprice < new_prime_cost {
            anyhow::bail!("販売単価が売上原価より低い設定はできません");
        }

        let now = Utc::now().naive_utc();

        let product = sqlx::query_as!(
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
            now
        )
        .fetch_one(&self.pool)
        .await?;

        Ok(product)
    }

    /// 商品を削除
    pub async fn delete_product(&self, prod_code: &str) -> Result<()> {
        // 存在確認
        let existing = self.get_product_by_id(prod_code).await?;
        if existing.is_none() {
            anyhow::bail!("商品が見つかりません");
        }

        sqlx::query!(r#"DELETE FROM "商品マスタ" WHERE "商品コード" = $1"#, prod_code)
            .execute(&self.pool)
            .await?;

        Ok(())
    }
}
