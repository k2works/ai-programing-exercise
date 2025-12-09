use sqlx::PgPool;

/// データベース接続プールを作成
pub async fn create_pool(database_url: &str) -> Result<PgPool, sqlx::Error> {
    PgPool::connect(database_url).await
}

#[cfg(test)]
mod tests {
    use super::*;
    use sqlx::Row;
    use rust_decimal::Decimal;
    use std::str::FromStr;
    use testcontainers::clients;
    use testcontainers_modules::postgres::Postgres;
    use testcontainers::Container;

    /// テスト用データベースヘルパー
    pub struct TestDatabase {
        pub pool: PgPool,
        _container: Container<'static, Postgres>,
    }

    impl TestDatabase {
        /// テスト用データベースをセットアップ
        pub async fn new() -> Self {
            let docker = Box::leak(Box::new(clients::Cli::default()));

            // PostgreSQLコンテナを起動
            let postgres = docker.run(Postgres::default());
            let port = postgres.get_host_port_ipv4(5432);

            // データベース接続URLを構築
            let database_url = format!(
                "postgresql://postgres:postgres@localhost:{}/postgres",
                port
            );

            // データベース接続プールを作成
            let pool = PgPool::connect(&database_url)
                .await
                .expect("Failed to create pool");

            // マイグレーションを実行
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

    #[tokio::test]
    async fn test_database_connection() {
        let db = TestDatabase::new().await;

        // 接続確認用の簡単なクエリ
        let result: (i32,) = sqlx::query_as("SELECT 1")
            .fetch_one(&db.pool)
            .await
            .expect("Failed to execute query");

        assert_eq!(result.0, 1);
    }

    #[tokio::test]
    async fn test_create_account() {
        // TestDatabaseヘルパーを使用
        let db = TestDatabase::new().await;

        // 1. 勘定科目を登録
        let result = sqlx::query(
            r#"
            INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
            VALUES ($1, $2, $3::account_type, $4)
            RETURNING "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目種別"::TEXT, "残高"
            "#
        )
        .bind("1000")
        .bind("現金")
        .bind("資産")
        .bind(Decimal::from_str("50000.00").unwrap())
        .fetch_one(&db.pool)
        .await;

        // 2. 取得したデータが期待通りか検証
        assert!(result.is_ok());
        let row = result.unwrap();
        assert_eq!(row.get::<String, _>("勘定科目コード"), "1000");
        assert_eq!(row.get::<String, _>("勘定科目名"), "現金");
        assert_eq!(row.get::<String, _>("勘定科目種別"), "資産");
        assert_eq!(
            row.get::<Decimal, _>("残高"),
            Decimal::from_str("50000.00").unwrap()
        );
    }

    #[tokio::test]
    async fn test_find_all_accounts() {
        let db = TestDatabase::new().await;

        // 1. 複数の勘定科目を登録
        insert_account(&db.pool, "1000", "現金", "資産", "50000.00").await;
        insert_account(&db.pool, "2000", "買掛金", "負債", "30000.00").await;
        insert_account(&db.pool, "3000", "資本金", "純資産", "100000.00").await;

        // 2. すべての勘定科目を取得
        let rows = sqlx::query(
            r#"
            SELECT "勘定科目コード", "勘定科目名", "勘定科目種別", "残高"
            FROM "勘定科目マスタ"
            ORDER BY "勘定科目コード"
            "#
        )
        .fetch_all(&db.pool)
        .await
        .expect("Failed to fetch accounts");

        // 3. 期待通りのデータが取得できるか検証
        assert_eq!(rows.len(), 3);
        assert_eq!(rows[0].get::<String, _>("勘定科目コード"), "1000");
        assert_eq!(rows[1].get::<String, _>("勘定科目コード"), "2000");
        assert_eq!(rows[2].get::<String, _>("勘定科目コード"), "3000");
    }

    #[tokio::test]
    async fn test_find_account_by_code() {
        let db = TestDatabase::new().await;

        // 1. テストデータを登録
        insert_account(&db.pool, "1000", "現金", "資産", "50000.00").await;

        // 2. コードで検索
        let row = sqlx::query(
            r#"
            SELECT "勘定科目コード", "勘定科目名", "勘定科目種別"::TEXT
            FROM "勘定科目マスタ"
            WHERE "勘定科目コード" = $1
            "#
        )
        .bind("1000")
        .fetch_one(&db.pool)
        .await
        .expect("Failed to find account");

        // 3. 正しいデータが取得できるか検証
        assert_eq!(row.get::<String, _>("勘定科目名"), "現金");
        assert_eq!(row.get::<String, _>("勘定科目種別"), "資産");
    }

    #[tokio::test]
    async fn test_update_account() {
        let db = TestDatabase::new().await;

        // 1. データを登録
        let account_id = insert_account(&db.pool, "1000", "現金", "資産", "50000.00").await;

        // 2. データを更新
        let updated = sqlx::query(
            r#"
            UPDATE "勘定科目マスタ"
            SET "勘定科目名" = $1, "残高" = $2, "更新日時" = CURRENT_TIMESTAMP
            WHERE "勘定科目ID" = $3
            "#
        )
        .bind("現金及び預金")
        .bind(Decimal::from_str("75000.00").unwrap())
        .bind(account_id)
        .execute(&db.pool)
        .await
        .expect("Failed to update account");

        assert_eq!(updated.rows_affected(), 1);

        // 3. 更新されたか検証
        let row = sqlx::query(
            r#"
            SELECT "勘定科目名", "残高"
            FROM "勘定科目マスタ"
            WHERE "勘定科目ID" = $1
            "#
        )
        .bind(account_id)
        .fetch_one(&db.pool)
        .await
        .expect("Failed to find updated account");

        assert_eq!(row.get::<String, _>("勘定科目名"), "現金及び預金");
        assert_eq!(
            row.get::<Decimal, _>("残高"),
            Decimal::from_str("75000.00").unwrap()
        );
    }

    // テストヘルパー関数
    async fn insert_account(
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
            "#
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

    // リファクタリングテスト
    #[tokio::test]
    async fn test_bspl_type_field() {
        let db = TestDatabase::new().await;

        // BSPL区分を設定して勘定科目を挿入
        let result = sqlx::query(
            r#"
            INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高")
            VALUES ($1, $2, $3::account_type, $4, $5)
            RETURNING "勘定科目コード", "BSPL区分"
            "#
        )
        .bind("1000")
        .bind("現金")
        .bind("資産")
        .bind("B")  // 貸借対照表
        .bind(Decimal::from_str("0").unwrap())
        .fetch_one(&db.pool)
        .await;

        if let Err(e) = &result {
            eprintln!("Error: {:?}", e);
        }
        assert!(result.is_ok());
        let row = result.unwrap();
        assert_eq!(row.get::<String, _>("BSPL区分"), "B");
    }

    #[tokio::test]
    async fn test_debit_credit_type_field() {
        let db = TestDatabase::new().await;

        // 貸借区分を設定して勘定科目を挿入
        let result = sqlx::query(
            r#"
            INSERT INTO "勘定科目マスタ"
            ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "貸借区分", "残高")
            VALUES ($1, $2, $3::account_type, $4, $5, $6)
            RETURNING "勘定科目コード", "貸借区分"
            "#
        )
        .bind("1000")
        .bind("現金")
        .bind("資産")
        .bind("B")
        .bind("D")  // 借方
        .bind(Decimal::from_str("0").unwrap())
        .fetch_one(&db.pool)
        .await;

        if let Err(e) = &result {
            eprintln!("Error: {:?}", e);
        }
        assert!(result.is_ok());
        let row = result.unwrap();
        assert_eq!(row.get::<String, _>("貸借区分"), "D");
    }

    #[tokio::test]
    async fn test_aggregation_type_field() {
        let db = TestDatabase::new().await;

        // 集計区分を設定して勘定科目を挿入
        let result = sqlx::query(
            r#"
            INSERT INTO "勘定科目マスタ"
            ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "貸借区分", "集計区分", "残高")
            VALUES ($1, $2, $3::account_type, $4, $5, $6, $7)
            RETURNING "勘定科目コード", "集計区分"
            "#
        )
        .bind("1000")
        .bind("現金")
        .bind("資産")
        .bind("B")
        .bind("D")
        .bind("D")  // 明細科目
        .bind(Decimal::from_str("0").unwrap())
        .fetch_one(&db.pool)
        .await;

        if let Err(e) = &result {
            eprintln!("Error: {:?}", e);
        }
        assert!(result.is_ok());
        let row = result.unwrap();
        assert_eq!(row.get::<String, _>("集計区分"), "D");
    }

    // 制約テスト
    #[tokio::test]
    async fn test_bspl_constraint_invalid_value() {
        let db = TestDatabase::new().await;

        // 不正な BSPL区分値を挿入しようとする
        let result = sqlx::query(
            r#"
            INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高")
            VALUES ($1, $2, $3::account_type, $4, $5)
            "#
        )
        .bind("1000")
        .bind("現金")
        .bind("資産")
        .bind("X")  // 不正な値
        .bind(Decimal::from_str("0").unwrap())
        .execute(&db.pool)
        .await;

        // CHECK制約違反でエラーになることを期待
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("check_bspl_distinction") || err_msg.contains("check constraint"));
    }

    #[tokio::test]
    async fn test_bspl_consistency_constraint() {
        let db = TestDatabase::new().await;

        // 資産科目にPL区分を設定しようとする（不整合）
        let result = sqlx::query(
            r#"
            INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "BSPL区分", "残高")
            VALUES ($1, $2, $3::account_type, $4, $5)
            "#
        )
        .bind("1000")
        .bind("現金")
        .bind("資産")
        .bind("P")  // 資産科目なのにPL区分（不整合）
        .bind(Decimal::from_str("0").unwrap())
        .execute(&db.pool)
        .await;

        // CHECK制約違反でエラーになることを期待
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("check_bspl_consistency") || err_msg.contains("check constraint"));
    }
}
