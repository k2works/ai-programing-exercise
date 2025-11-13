// src/lib.rs
use dotenv::dotenv;
use sqlx::PgPool;
use std::env;

pub mod entity;
pub mod repository;

// テストサポート（テスト時のみ有効）
#[cfg(test)]
pub mod test_support;

/// データベース接続プールを作成
pub async fn create_pool() -> Result<PgPool, sqlx::Error> {
    dotenv().ok();
    let database_url = env::var("DATABASE_URL").expect("DATABASE_URL must be set");

    PgPool::connect(&database_url).await
}
