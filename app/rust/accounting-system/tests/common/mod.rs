use rust_decimal::Decimal;
use sqlx::{PgPool, Row};
use std::str::FromStr;
use testcontainers::clients;
use testcontainers::{Container, GenericImage};
use testcontainers_modules::postgres::Postgres;

/// Test database helper
pub struct TestDatabase {
    pub pool: PgPool,
    _container: Container<'static, Postgres>,
}

impl TestDatabase {
    /// Setup test database
    pub async fn new() -> Self {
        let docker = Box::leak(Box::new(clients::Cli::default()));

        let postgres = docker.run(Postgres::default());
        let port = postgres.get_host_port_ipv4(5432);

        let database_url = format!("postgresql://postgres:postgres@localhost:{}/postgres", port);

        let pool = PgPool::connect(&database_url)
            .await
            .expect("Failed to create pool");

        sqlx::migrate!("./migrations")
            .run(&pool)
            .await
            .expect("Failed to run migrations");

        Self {
            pool,
            _container: postgres,
        }
    }
}

/// Test RabbitMQ helper
pub struct TestRabbitMQ {
    pub url: String,
    _container: Container<'static, GenericImage>,
}

impl TestRabbitMQ {
    /// Setup test RabbitMQ
    pub fn new() -> Self {
        let docker = Box::leak(Box::new(clients::Cli::default()));

        let rabbitmq =
            docker.run(GenericImage::new("rabbitmq", "3.12-alpine").with_exposed_port(5672));

        let port = rabbitmq.get_host_port_ipv4(5672);
        let url = format!("amqp://guest:guest@localhost:{}", port);

        Self {
            url,
            _container: rabbitmq,
        }
    }
}

/// Helper function to insert test account
pub async fn insert_account(
    pool: &PgPool,
    code: &str,
    name: &str,
    account_type: &str,
    balance: &str,
) -> i32 {
    let row = sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
        VALUES ($1, $2, $3::account_type, $4)
        RETURNING "勘定科目ID"
        "#,
    )
    .bind(code)
    .bind(name)
    .bind(account_type)
    .bind(Decimal::from_str(balance).unwrap())
    .fetch_one(pool)
    .await
    .expect("Failed to insert account");

    row.get::<i32, _>("勘定科目ID")
}
