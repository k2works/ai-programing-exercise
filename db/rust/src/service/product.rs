//! 商品サービス層
//! ビジネスロジックとトランザクション制御

use crate::dto::product::{CreateProductRequest, ProductResponse, UpdateProductRequest};
use crate::entity::Product;
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
        let now = Utc::now().naive_utc();

        // DTO から Entity への変換
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
            wide_use_type: None,
            stock_manage_type: request.stock_managed,
            stock_reserve_type: request.stock_reserve,
            sup_code: request.sup_code.clone(),
            sup_sub_no: request.sup_seq_num,
            create_date: now,
            creator: None,
            update_date: now,
            updater: None,
        };

        // ビジネスルールの検証
        entity.validate().map_err(|e| anyhow::anyhow!(e))?;

        // Repository で Entity を保存
        ProductRepository::create(&self.pool, &entity).await?;

        // 保存された Entity を取得
        let saved_entity = ProductRepository::find_by_code(&self.pool, &entity.prod_code).await?;

        // Entity から DTO への変換
        Ok(ProductResponse::from_entity(saved_entity))
    }

    /// すべての商品を取得
    pub async fn get_all_products(&self) -> Result<Vec<ProductResponse>> {
        // Repository で Entity を全件取得
        let entities = ProductRepository::find_all(&self.pool).await?;

        // Entity から DTO への変換
        let products = entities.into_iter().map(ProductResponse::from_entity).collect();

        Ok(products)
    }

    /// IDで商品を取得
    pub async fn get_product_by_id(&self, prod_code: &str) -> Result<Option<ProductResponse>> {
        // Repository で Entity を取得
        match ProductRepository::find_by_code(&self.pool, prod_code).await {
            Ok(entity) => {
                // Entity から DTO への変換
                Ok(Some(ProductResponse::from_entity(entity)))
            }
            Err(sqlx::Error::RowNotFound) => Ok(None),
            Err(e) => Err(e.into()),
        }
    }

    /// 商品を更新
    pub async fn update_product(
        &self,
        prod_code: &str,
        request: UpdateProductRequest,
    ) -> Result<ProductResponse> {
        // 既存の Entity を取得
        let mut entity = ProductRepository::find_by_code(&self.pool, prod_code).await?;

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

        let now = Utc::now().naive_utc();
        entity.update_date = now;

        // ビジネスルールの検証
        entity.validate().map_err(|e| anyhow::anyhow!(e))?;

        // Repository で Entity を更新
        ProductRepository::update(&self.pool, &entity).await?;

        // 更新された Entity を取得
        let updated_entity = ProductRepository::find_by_code(&self.pool, prod_code).await?;

        // Entity から DTO への変換
        Ok(ProductResponse::from_entity(updated_entity))
    }

    /// 商品を削除
    pub async fn delete_product(&self, prod_code: &str) -> Result<()> {
        // 存在確認（Repository の find_by_code で Entity を取得）
        ProductRepository::find_by_code(&self.pool, prod_code).await?;

        // Repository で Entity を削除
        ProductRepository::delete(&self.pool, prod_code).await?;

        Ok(())
    }
}
