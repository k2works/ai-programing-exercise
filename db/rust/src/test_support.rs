//! テストサポート: TestcontainersでPostgreSQLを起動し、接続プールを提供する
#![cfg(test)]

use sqlx::PgPool;
use testcontainers::clients::Cli;
use testcontainers::Container;
use testcontainers_modules::postgres::Postgres;

/// テスト用にPostgreSQLコンテナを起動し、コールバックに`PgPool`を渡して実行する。
/// コールバックの実行が終わるとコンテナは自動的に破棄される。
pub async fn with_test_pool<F, Fut, T>(f: F) -> T
where
    F: FnOnce(PgPool) -> Fut,
    Fut: std::future::Future<Output = T>,
{
    // Dockerクライアントを用意
    let docker = Cli::default();

    // Postgresコンテナを起動（testcontainers-modulesのデフォルト設定を利用）
    let container: Container<'_, Postgres> = docker.run(Postgres::default());

    // ホスト側ポートを取得して接続URLを構成
    let port = container.get_host_port_ipv4(5432);
    let database_url =
        format!("postgres://postgres:postgres@127.0.0.1:{port}/postgres", port = port);

    // 接続プール作成
    let pool =
        PgPool::connect(&database_url).await.expect("failed to connect to test postgres container");

    // マイグレーションを適用（db/rust/migrations 配下）
    sqlx::migrate!("./migrations")
        .run(&pool)
        .await
        .expect("failed to run migrations on test database");

    // コールバックを実行
    let out = f(pool).await;

    // `container` と `pool` はここでドロップされ、コンテナは停止・破棄される
    out
}
