// src/lib.rs
use dotenv::dotenv;
use sqlx::PgPool;
use std::env;

pub mod dto;
pub mod entity;
pub mod error;
pub mod handler;
pub mod repository;
pub mod service;

// テストサポート（テスト時または test-support フィーチャー有効時に公開）
#[cfg(any(test, feature = "test-support"))]
pub mod test_support;

/// データベース接続プールを作成
pub async fn create_pool() -> Result<PgPool, sqlx::Error> {
    dotenv().ok();
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");

    PgPool::connect(&database_url).await
}
