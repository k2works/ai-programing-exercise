use sqlx::PgPool;

/// データベース接続プールを作成
pub async fn create_pool(database_url: &str) -> Result<PgPool, sqlx::Error> {
    PgPool::connect(database_url).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use testcontainers::clients;
    use testcontainers_modules::postgres::Postgres;

    #[tokio::test]
    async fn test_database_connection() {
        // Dockerクライアントを作成
        let docker = clients::Cli::default();

        // PostgreSQLコンテナを起動
        let postgres_container = docker.run(Postgres::default());
        let port = postgres_container.get_host_port_ipv4(5432);

        // データベース接続URLを構築
        let database_url = format!(
            "postgresql://postgres:postgres@localhost:{}/postgres",
            port
        );

        // データベースに接続
        let pool = create_pool(&database_url).await
            .expect("Failed to create pool");

        // 接続確認用の簡単なクエリ
        let result: (i32,) = sqlx::query_as("SELECT 1")
            .fetch_one(&pool)
            .await
            .expect("Failed to execute query");

        assert_eq!(result.0, 1);

        pool.close().await;
        // テスト終了時にコンテナは自動的に削除される
    }
}
