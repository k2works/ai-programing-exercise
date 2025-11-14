//! DTO (Data Transfer Object) 層
//! APIリクエスト・レスポンスの型定義

pub mod product;

use serde::Serialize;
use utoipa::ToSchema;

/// 共通エラーレスポンス
#[derive(Debug, Serialize, ToSchema)]
pub struct ErrorResponse {
    pub error: String,
    pub details: Option<String>,
}

impl ErrorResponse {
    pub fn new(error: impl Into<String>) -> Self {
        Self { error: error.into(), details: None }
    }

    pub fn with_details(error: impl Into<String>, details: impl Into<String>) -> Self {
        Self { error: error.into(), details: Some(details.into()) }
    }
}
