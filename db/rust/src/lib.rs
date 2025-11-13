// src/lib.rs
use sqlx::PgPool;
use dotenv::dotenv;
use std::env;

/// データベース接続プールを作成
pub async fn create_pool() -> Result<PgPool, sqlx::Error> {
    dotenv().ok();
    let database_url = env::var("DATABASE_URL")
        .expect("DATABASE_URL must be set");

    PgPool::connect(&database_url).await
}

#[cfg(test)]
mod tests {
    use super::*;

    /// テスト用のデータベース接続プールを作成
    async fn setup() -> PgPool {
        create_pool().await.expect("Failed to create pool")
    }

    #[tokio::test]
    async fn test_database_connection() {
        let pool = setup().await;

        // 接続テスト
        let result = sqlx::query("SELECT 1")
            .fetch_one(&pool)
            .await;

        assert!(result.is_ok());
    }
}
