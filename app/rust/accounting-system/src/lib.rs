pub mod domain;
pub mod repositories;

use sqlx::PgPool;

/// データベース接続プールを作成
pub async fn create_pool(database_url: &str) -> Result<PgPool, sqlx::Error> {
    PgPool::connect(database_url).await
}
