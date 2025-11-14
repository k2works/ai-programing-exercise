//! 商品ハンドラー
//! 商品APIのHTTPハンドラー関数

use crate::dto::product::{CreateProductRequest, ProductResponse, UpdateProductRequest};
use crate::error::AppError;
use crate::service::ProductService;
use axum::{
    extract::{Path, State},
    http::StatusCode,
    Json,
};
use std::sync::Arc;
use validator::Validate;

/// 商品を作成
#[utoipa::path(
    post,
    path = "/products",
    request_body = CreateProductRequest,
    responses(
        (status = 201, description = "商品が正常に作成されました", body = ProductResponse),
        (status = 400, description = "バリデーションエラー"),
        (status = 500, description = "サーバーエラー")
    ),
    tag = "products"
)]
pub async fn create_product(
    State(service): State<Arc<ProductService>>,
    Json(request): Json<CreateProductRequest>,
) -> Result<(StatusCode, Json<ProductResponse>), AppError> {
    // バリデーション
    request.validate()?;

    let product = service.create_product(request).await?;
    Ok((StatusCode::CREATED, Json(product)))
}

/// すべての商品を取得
#[utoipa::path(
    get,
    path = "/products",
    responses(
        (status = 200, description = "商品一覧を取得", body = Vec<ProductResponse>),
        (status = 500, description = "サーバーエラー")
    ),
    tag = "products"
)]
pub async fn get_all_products(
    State(service): State<Arc<ProductService>>,
) -> Result<Json<Vec<ProductResponse>>, AppError> {
    let products = service.get_all_products().await?;
    Ok(Json(products))
}

/// IDで商品を取得
#[utoipa::path(
    get,
    path = "/products/{prod_code}",
    params(
        ("prod_code" = String, Path, description = "商品コード")
    ),
    responses(
        (status = 200, description = "商品を取得", body = ProductResponse),
        (status = 404, description = "商品が見つかりません"),
        (status = 500, description = "サーバーエラー")
    ),
    tag = "products"
)]
pub async fn get_product_by_id(
    State(service): State<Arc<ProductService>>,
    Path(prod_code): Path<String>,
) -> Result<Json<ProductResponse>, AppError> {
    let product = service.get_product_by_id(&prod_code).await?;

    match product {
        Some(p) => Ok(Json(p)),
        None => Err(AppError::NotFound("商品が見つかりません".to_string())),
    }
}

/// 商品を更新
#[utoipa::path(
    put,
    path = "/products/{prod_code}",
    params(
        ("prod_code" = String, Path, description = "商品コード")
    ),
    request_body = UpdateProductRequest,
    responses(
        (status = 200, description = "商品が正常に更新されました", body = ProductResponse),
        (status = 400, description = "バリデーションエラー"),
        (status = 404, description = "商品が見つかりません"),
        (status = 500, description = "サーバーエラー")
    ),
    tag = "products"
)]
pub async fn update_product(
    State(service): State<Arc<ProductService>>,
    Path(prod_code): Path<String>,
    Json(request): Json<UpdateProductRequest>,
) -> Result<Json<ProductResponse>, AppError> {
    // バリデーション
    request.validate()?;

    let product = service.update_product(&prod_code, request).await?;
    Ok(Json(product))
}

/// 商品を削除
#[utoipa::path(
    delete,
    path = "/products/{prod_code}",
    params(
        ("prod_code" = String, Path, description = "商品コード")
    ),
    responses(
        (status = 204, description = "商品が正常に削除されました"),
        (status = 404, description = "商品が見つかりません"),
        (status = 500, description = "サーバーエラー")
    ),
    tag = "products"
)]
pub async fn delete_product(
    State(service): State<Arc<ProductService>>,
    Path(prod_code): Path<String>,
) -> Result<StatusCode, AppError> {
    service.delete_product(&prod_code).await?;
    Ok(StatusCode::NO_CONTENT)
}
