pub mod domain;
pub mod repositories;

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

    // 課税取引コードテスト
    #[tokio::test]
    async fn test_add_tax_code_to_account() {
        let db = TestDatabase::new().await;

        // 課税取引コード付きの勘定科目を挿入
        let result = sqlx::query(
            r#"
            INSERT INTO "勘定科目マスタ"
            ("勘定科目コード", "勘定科目名", "勘定科目種別", "課税取引コード", "残高")
            VALUES ($1, $2, $3::account_type, $4, $5)
            RETURNING "勘定科目コード", "課税取引コード"
            "#
        )
        .bind("5000")
        .bind("売上高")
        .bind("収益")
        .bind("01")  // 課税取引コード（課税売上）
        .bind(Decimal::from_str("0").unwrap())
        .fetch_one(&db.pool)
        .await;

        assert!(result.is_ok());
        let row = result.unwrap();
        assert_eq!(row.get::<String, _>("勘定科目コード"), "5000");
        assert_eq!(row.get::<Option<String>, _>("課税取引コード"), Some("01".to_string()));
    }

    #[tokio::test]
    async fn test_account_without_tax_code() {
        let db = TestDatabase::new().await;

        // 課税取引コードなしの勘定科目を挿入（固定資産など）
        let result = sqlx::query(
            r#"
            INSERT INTO "勘定科目マスタ"
            ("勘定科目コード", "勘定科目名", "勘定科目種別", "残高")
            VALUES ($1, $2, $3::account_type, $4)
            RETURNING "勘定科目コード", "課税取引コード"
            "#
        )
        .bind("1500")
        .bind("建物")
        .bind("資産")
        .bind(Decimal::from_str("0").unwrap())
        .fetch_one(&db.pool)
        .await;

        assert!(result.is_ok());
        let row = result.unwrap();
        assert_eq!(row.get::<String, _>("勘定科目コード"), "1500");
        assert_eq!(row.get::<Option<String>, _>("課税取引コード"), None);
    }

    #[tokio::test]
    async fn test_query_accounts_by_tax_code() {
        let db = TestDatabase::new().await;

        // 複数の勘定科目を挿入
        sqlx::query(
            r#"
            INSERT INTO "勘定科目マスタ" ("勘定科目コード", "勘定科目名", "勘定科目種別", "課税取引コード", "残高")
            VALUES
                ('5000', '売上高', '収益'::account_type, '01', 0),
                ('5100', 'サービス売上', '収益'::account_type, '01', 0),
                ('5200', '輸出売上', '収益'::account_type, '02', 0),
                ('5300', '非課税売上', '収益'::account_type, NULL, 0)
            "#
        )
        .execute(&db.pool)
        .await
        .expect("Failed to insert test data");

        // 課税取引コード '01' の勘定科目を検索
        let rows = sqlx::query(
            r#"
            SELECT "勘定科目コード", "勘定科目名", "課税取引コード"
            FROM "勘定科目マスタ"
            WHERE "課税取引コード" = $1
            ORDER BY "勘定科目コード"
            "#
        )
        .bind("01")
        .fetch_all(&db.pool)
        .await
        .expect("Failed to query accounts");

        assert_eq!(rows.len(), 2);
        assert_eq!(rows[0].get::<String, _>("勘定科目コード"), "5000");
        assert_eq!(rows[1].get::<String, _>("勘定科目コード"), "5100");
    }

    // 勘定科目構成マスタテスト
    #[tokio::test]
    async fn test_create_account_structure() {
        let db = TestDatabase::new().await;

        // まず勘定科目を登録
        insert_account(&db.pool, "11", "資産の部", "資産", "0").await;

        // 勘定科目構成を登録
        let result = sqlx::query(
            r#"
            INSERT INTO "勘定科目構成マスタ"
            ("勘定科目コード", "勘定科目パス", "階層レベル", "親科目コード", "表示順序")
            VALUES ($1, $2, $3, $4, $5)
            RETURNING "勘定科目コード", "勘定科目パス", "階層レベル"
            "#
        )
        .bind("11")
        .bind("11")
        .bind(1)
        .bind(None::<String>)
        .bind(0)
        .fetch_one(&db.pool)
        .await;

        assert!(result.is_ok());
        let row = result.unwrap();
        assert_eq!(row.get::<String, _>("勘定科目コード"), "11");
        assert_eq!(row.get::<String, _>("勘定科目パス"), "11");
        assert_eq!(row.get::<i32, _>("階層レベル"), 1);
    }

    #[tokio::test]
    async fn test_create_hierarchical_accounts() {
        let db = TestDatabase::new().await;

        // 階層構造の勘定科目を登録
        insert_account(&db.pool, "11", "資産の部", "資産", "0").await;
        insert_account(&db.pool, "11000", "流動資産", "資産", "0").await;
        insert_account(&db.pool, "11190", "現金及び預金", "資産", "0").await;
        insert_account(&db.pool, "11110", "現金", "資産", "50000").await;

        // 勘定科目構成を登録
        sqlx::query(
            r#"
            INSERT INTO "勘定科目構成マスタ"
            ("勘定科目コード", "勘定科目パス", "階層レベル", "親科目コード", "表示順序")
            VALUES
                ('11', '11', 1, NULL, 0),
                ('11000', '11~11000', 2, '11', 1),
                ('11190', '11~11000~11190', 3, '11000', 1),
                ('11110', '11~11000~11190~11110', 4, '11190', 1)
            "#
        )
        .execute(&db.pool)
        .await
        .expect("Failed to insert account structures");

        // 特定パスで検索
        let rows = sqlx::query(
            r#"
            SELECT "勘定科目コード", "勘定科目パス", "階層レベル"
            FROM "勘定科目構成マスタ"
            WHERE "勘定科目パス" LIKE '11~11000%'
            ORDER BY "勘定科目パス"
            "#
        )
        .fetch_all(&db.pool)
        .await
        .expect("Failed to query");

        assert_eq!(rows.len(), 3); // 11000, 11190, 11110
        assert_eq!(rows[0].get::<String, _>("勘定科目コード"), "11000");
        assert_eq!(rows[1].get::<String, _>("勘定科目コード"), "11190");
        assert_eq!(rows[2].get::<String, _>("勘定科目コード"), "11110");
    }

    // 課税取引マスタテスト
    #[tokio::test]
    async fn test_tax_transaction_initial_data() {
        let db = TestDatabase::new().await;

        // 初期データが投入されているか確認
        let rows = sqlx::query(
            r#"
            SELECT "課税取引コード", "課税取引名", "税率"
            FROM "課税取引マスタ"
            ORDER BY "課税取引コード"
            "#
        )
        .fetch_all(&db.pool)
        .await
        .expect("Failed to query tax transactions");

        assert_eq!(rows.len(), 4);
        assert_eq!(rows[0].get::<String, _>("課税取引コード"), "01");
        assert_eq!(rows[0].get::<String, _>("課税取引名"), "課税");
        assert_eq!(rows[0].get::<Decimal, _>("税率"), Decimal::from_str("0.10").unwrap());
    }

    #[tokio::test]
    async fn test_tax_rate_constraint() {
        let db = TestDatabase::new().await;

        // 不正な税率を挿入しようとする（1を超える）
        let result = sqlx::query(
            r#"
            INSERT INTO "課税取引マスタ" ("課税取引コード", "課税取引名", "税率")
            VALUES ($1, $2, $3)
            "#
        )
        .bind("99")
        .bind("無効税率")
        .bind(Decimal::from_str("1.5").unwrap())
        .execute(&db.pool)
        .await;

        // CHECK制約違反でエラーになることを期待
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("check_tax_rate") || err_msg.contains("check constraint"));
    }

    #[tokio::test]
    async fn test_account_tax_code_foreign_key() {
        let db = TestDatabase::new().await;

        // 課税取引コード付きの勘定科目を挿入（外部キー制約があるので参照整合性チェック）
        let result = sqlx::query(
            r#"
            INSERT INTO "勘定科目マスタ"
            ("勘定科目コード", "勘定科目名", "勘定科目種別", "課税取引コード", "残高")
            VALUES ($1, $2, $3::account_type, $4, $5)
            "#
        )
        .bind("5000")
        .bind("売上高")
        .bind("収益")
        .bind("01")  // 存在する課税取引コード
        .bind(Decimal::from_str("0").unwrap())
        .execute(&db.pool)
        .await;

        assert!(result.is_ok());

        // 存在しない課税取引コードで挿入を試みる
        let invalid_result = sqlx::query(
            r#"
            INSERT INTO "勘定科目マスタ"
            ("勘定科目コード", "勘定科目名", "勘定科目種別", "課税取引コード", "残高")
            VALUES ($1, $2, $3::account_type, $4, $5)
            "#
        )
        .bind("5100")
        .bind("その他売上")
        .bind("収益")
        .bind("99")  // 存在しない課税取引コード
        .bind(Decimal::from_str("0").unwrap())
        .execute(&db.pool)
        .await;

        // 外部キー制約違反でエラーになることを期待
        assert!(invalid_result.is_err());
        let err_msg = invalid_result.unwrap_err().to_string();
        assert!(err_msg.contains("foreign key") || err_msg.contains("fk_account_tax_code"));
    }

    // Account Repository tests
    use crate::domain::account::Account;
    use crate::repositories::account;
    use rust_decimal_macros::dec;

    #[tokio::test]
    async fn test_account_repository_insert() {
        let db = TestDatabase::new().await;

        let mut new_account = Account::new("1000".to_string(), "現金".to_string(), "資産".to_string(), false);
        new_account.balance = dec!(100000.00);

        let account_id = account::insert(&db.pool, &new_account).await.unwrap();
        assert!(account_id > 0);

        let found = account::find_by_code(&db.pool, "1000").await.unwrap();
        assert!(found.is_some());
        let found = found.unwrap();
        assert_eq!(found.account_name, "現金");
        assert_eq!(found.account_type, "資産");
        assert_eq!(found.balance, dec!(100000.00));
    }

    #[tokio::test]
    async fn test_account_repository_find_all() {
        let db = TestDatabase::new().await;

        account::insert(&db.pool, &Account::new("1000".to_string(), "現金".to_string(), "資産".to_string(), false))
            .await
            .unwrap();
        account::insert(&db.pool, &Account::new("2000".to_string(), "買掛金".to_string(), "負債".to_string(), false))
            .await
            .unwrap();
        account::insert(&db.pool, &Account::new("3000".to_string(), "資本金".to_string(), "純資産".to_string(), false))
            .await
            .unwrap();

        let all = account::find_all(&db.pool).await.unwrap();
        assert_eq!(all.len(), 3);
        assert_eq!(all[0].account_code, "1000");
        assert_eq!(all[1].account_code, "2000");
        assert_eq!(all[2].account_code, "3000");
    }

    #[tokio::test]
    async fn test_account_repository_find_by_type() {
        let db = TestDatabase::new().await;

        account::insert(&db.pool, &Account::new("1000".to_string(), "現金".to_string(), "資産".to_string(), false))
            .await
            .unwrap();
        account::insert(&db.pool, &Account::new("1100".to_string(), "普通預金".to_string(), "資産".to_string(), false))
            .await
            .unwrap();
        account::insert(&db.pool, &Account::new("2000".to_string(), "買掛金".to_string(), "負債".to_string(), false))
            .await
            .unwrap();

        let assets = account::find_by_type(&db.pool, "資産").await.unwrap();
        assert_eq!(assets.len(), 2);
        assert!(assets.iter().any(|a| a.account_name == "現金"));
        assert!(assets.iter().any(|a| a.account_name == "普通預金"));
    }

    #[tokio::test]
    async fn test_account_repository_summary_and_detail() {
        let db = TestDatabase::new().await;

        // 合計科目
        account::insert(&db.pool, &Account::new("11".to_string(), "資産の部".to_string(), "資産".to_string(), true))
            .await
            .unwrap();
        account::insert(&db.pool, &Account::new("11000".to_string(), "流動資産".to_string(), "資産".to_string(), true))
            .await
            .unwrap();

        // 明細科目
        account::insert(&db.pool, &Account::new("11110".to_string(), "現金".to_string(), "資産".to_string(), false))
            .await
            .unwrap();
        account::insert(&db.pool, &Account::new("11120".to_string(), "普通預金".to_string(), "資産".to_string(), false))
            .await
            .unwrap();

        // 合計科目のみ取得
        let summary_accounts = account::find_summary_accounts(&db.pool).await.unwrap();
        assert_eq!(summary_accounts.len(), 2);
        assert!(summary_accounts.iter().all(|a| a.is_summary_account));

        // 明細科目のみ取得
        let detail_accounts = account::find_detail_accounts(&db.pool).await.unwrap();
        assert_eq!(detail_accounts.len(), 2);
        assert!(detail_accounts.iter().all(|a| !a.is_summary_account));
    }

    #[tokio::test]
    async fn test_account_repository_update_balance() {
        let db = TestDatabase::new().await;

        let mut account_data = Account::new("1000".to_string(), "現金".to_string(), "資産".to_string(), false);
        account_data.balance = dec!(50000.00);
        account::insert(&db.pool, &account_data).await.unwrap();

        // 残高を更新
        account::update_balance(&db.pool, "1000", dec!(75000.00))
            .await
            .unwrap();

        let updated = account::find_by_code(&db.pool, "1000")
            .await
            .unwrap()
            .unwrap();
        assert_eq!(updated.balance, dec!(75000.00));
    }

    #[tokio::test]
    async fn test_account_repository_delete() {
        let db = TestDatabase::new().await;

        account::insert(&db.pool, &Account::new("1000".to_string(), "現金".to_string(), "資産".to_string(), false))
            .await
            .unwrap();

        account::delete(&db.pool, "1000").await.unwrap();

        let deleted = account::find_by_code(&db.pool, "1000").await.unwrap();
        assert!(deleted.is_none());
    }
}
