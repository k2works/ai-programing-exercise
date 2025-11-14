//! 商品サービス層
//! ビジネスロジックとトランザクション制御

use crate::dto::product::{CreateProductRequest, ProductResponse, UpdateProductRequest};
use crate::repository::ProductRepository;
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

        let product = ProductRepository::create_as_dto(&self.pool, &request, now, now).await?;

        Ok(product)
    }

    /// すべての商品を取得
    pub async fn get_all_products(&self) -> Result<Vec<ProductResponse>> {
        let products = ProductRepository::find_all_as_dto(&self.pool).await?;
        Ok(products)
    }

    /// IDで商品を取得
    pub async fn get_product_by_id(&self, prod_code: &str) -> Result<Option<ProductResponse>> {
        let product = ProductRepository::find_by_code_as_dto(&self.pool, prod_code).await?;
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

        let product =
            ProductRepository::update_as_dto(&self.pool, prod_code, &request, now).await?;

        Ok(product)
    }

    /// 商品を削除
    pub async fn delete_product(&self, prod_code: &str) -> Result<()> {
        // 存在確認
        let existing = self.get_product_by_id(prod_code).await?;
        if existing.is_none() {
            anyhow::bail!("商品が見つかりません");
        }

        ProductRepository::delete_by_code(&self.pool, prod_code).await?;

        Ok(())
    }
}
