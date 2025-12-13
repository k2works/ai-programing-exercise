use async_trait::async_trait;
use sqlx::PgPool;

use crate::application::ports::output::account_repository::AccountRepository;
use crate::domain::account::Account;

/// 勘定科目リポジトリの実装（Output Adapter）
pub struct AccountRepositoryImpl {
    pool: PgPool,
}

impl AccountRepositoryImpl {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }
}

#[async_trait]
impl AccountRepository for AccountRepositoryImpl {
    async fn find_all(&self) -> Result<Vec<Account>, Box<dyn std::error::Error>> {
        let accounts = sqlx::query_as::<_, Account>(
            r#"
            SELECT
                "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
                "勘定科目種別"::TEXT as "勘定科目種別",
                "合計科目", "BSPL区分", "取引要素区分", "費用区分",
                "表示順序", "集計対象", "課税取引コード", "残高",
                "作成日時", "更新日時"
            FROM "勘定科目マスタ"
            ORDER BY "勘定科目コード"
            "#,
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(accounts)
    }

    async fn find_by_code(
        &self,
        code: &str,
    ) -> Result<Option<Account>, Box<dyn std::error::Error>> {
        let account = sqlx::query_as::<_, Account>(
            r#"
            SELECT
                "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
                "勘定科目種別"::TEXT as "勘定科目種別",
                "合計科目", "BSPL区分", "取引要素区分", "費用区分",
                "表示順序", "集計対象", "課税取引コード", "残高",
                "作成日時", "更新日時"
            FROM "勘定科目マスタ"
            WHERE "勘定科目コード" = $1
            "#,
        )
        .bind(code)
        .fetch_optional(&self.pool)
        .await?;

        Ok(account)
    }

    async fn create(&self, account: Account) -> Result<Account, Box<dyn std::error::Error>> {
        let created = sqlx::query_as::<_, Account>(
            r#"
            INSERT INTO "勘定科目マスタ" (
                "勘定科目コード", "勘定科目名", "勘定科目カナ", "勘定科目種別",
                "合計科目", "BSPL区分", "取引要素区分", "費用区分",
                "表示順序", "集計対象", "課税取引コード", "残高"
            ) VALUES ($1, $2, $3, $4::account_type, $5, $6, $7, $8, $9, $10, $11, $12)
            RETURNING
                "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
                "勘定科目種別"::TEXT as "勘定科目種別",
                "合計科目", "BSPL区分", "取引要素区分", "費用区分",
                "表示順序", "集計対象", "課税取引コード", "残高",
                "作成日時", "更新日時"
            "#,
        )
        .bind(&account.account_code)
        .bind(&account.account_name)
        .bind(&account.account_name_kana)
        .bind(&account.account_type)
        .bind(account.is_summary_account)
        .bind(&account.bspl_type)
        .bind(&account.transaction_element_type)
        .bind(&account.expense_type)
        .bind(account.display_order)
        .bind(account.is_aggregation_target)
        .bind(&account.tax_code)
        .bind(account.balance)
        .fetch_one(&self.pool)
        .await?;

        Ok(created)
    }

    async fn update(&self, account: Account) -> Result<Account, Box<dyn std::error::Error>> {
        let updated = sqlx::query_as::<_, Account>(
            r#"
            UPDATE "勘定科目マスタ"
            SET
                "勘定科目名" = $2,
                "勘定科目カナ" = $3,
                "勘定科目種別" = $4::account_type,
                "合計科目" = $5,
                "BSPL区分" = $6,
                "取引要素区分" = $7,
                "費用区分" = $8,
                "表示順序" = $9,
                "集計対象" = $10,
                "課税取引コード" = $11,
                "残高" = $12,
                "更新日時" = CURRENT_TIMESTAMP
            WHERE "勘定科目コード" = $1
            RETURNING
                "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
                "勘定科目種別"::TEXT as "勘定科目種別",
                "合計科目", "BSPL区分", "取引要素区分", "費用区分",
                "表示順序", "集計対象", "課税取引コード", "残高",
                "作成日時", "更新日時"
            "#,
        )
        .bind(&account.account_code)
        .bind(&account.account_name)
        .bind(&account.account_name_kana)
        .bind(&account.account_type)
        .bind(account.is_summary_account)
        .bind(&account.bspl_type)
        .bind(&account.transaction_element_type)
        .bind(&account.expense_type)
        .bind(account.display_order)
        .bind(account.is_aggregation_target)
        .bind(&account.tax_code)
        .bind(account.balance)
        .fetch_one(&self.pool)
        .await?;

        Ok(updated)
    }
}
