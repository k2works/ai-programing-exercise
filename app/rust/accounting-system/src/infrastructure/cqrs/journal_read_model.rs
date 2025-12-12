use chrono::{DateTime, NaiveDate, Utc};
use rust_decimal::Decimal;
use serde::{Deserialize, Serialize};
use sqlx::PgPool;

/// 仕訳の読み取りモデル
#[derive(Debug, Clone, Serialize, Deserialize, sqlx::FromRow)]
pub struct JournalReadModel {
    pub journal_id: String,
    pub journal_date: NaiveDate,
    pub description: String,
    pub fiscal_year: i32,
    pub status: String,
    pub total_debit: i64,
    pub total_credit: i64,
    pub entry_count: i32,
    pub approved_by: Option<String>,
    pub approved_at: Option<DateTime<Utc>>,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
}

/// 仕訳明細の読み取りモデル
#[derive(Debug, Clone, Serialize, Deserialize, sqlx::FromRow)]
pub struct JournalEntryReadModel {
    pub entry_id: i64,
    pub journal_id: String,
    pub account_code: String,
    pub account_name: Option<String>,
    pub debit_amount: Option<i64>,
    pub credit_amount: Option<i64>,
    pub description: Option<String>,
    pub line_number: i32,
    pub created_at: DateTime<Utc>,
}

/// 読み取りモデルリポジトリ
pub struct JournalReadModelRepository {
    pool: PgPool,
}

impl JournalReadModelRepository {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    /// 読み取りモデルを作成または更新
    pub async fn upsert(&self, model: &JournalReadModel) -> Result<(), sqlx::Error> {
        sqlx::query!(
            r#"
            INSERT INTO journal_read_model (
                journal_id, journal_date, description, fiscal_year,
                status, total_debit, total_credit, entry_count,
                approved_by, approved_at, created_at, updated_at
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
            ON CONFLICT (journal_id)
            DO UPDATE SET
                journal_date = EXCLUDED.journal_date,
                description = EXCLUDED.description,
                fiscal_year = EXCLUDED.fiscal_year,
                status = EXCLUDED.status,
                total_debit = EXCLUDED.total_debit,
                total_credit = EXCLUDED.total_credit,
                entry_count = EXCLUDED.entry_count,
                approved_by = EXCLUDED.approved_by,
                approved_at = EXCLUDED.approved_at,
                updated_at = EXCLUDED.updated_at
            "#,
            model.journal_id,
            model.journal_date,
            model.description,
            model.fiscal_year,
            model.status,
            model.total_debit,
            model.total_credit,
            model.entry_count,
            model.approved_by,
            model.approved_at,
            model.created_at,
            model.updated_at
        )
        .execute(&self.pool)
        .await?;

        Ok(())
    }

    /// IDで検索
    pub async fn find_by_id(&self, id: &str) -> Result<Option<JournalReadModel>, sqlx::Error> {
        let result = sqlx::query_as!(
            JournalReadModel,
            r#"
            SELECT
                journal_id,
                journal_date,
                description,
                fiscal_year,
                status,
                total_debit,
                total_credit,
                entry_count,
                approved_by,
                approved_at,
                created_at,
                updated_at
            FROM journal_read_model
            WHERE journal_id = $1
            "#,
            id
        )
        .fetch_optional(&self.pool)
        .await?;

        Ok(result)
    }

    /// ステータスで一覧取得
    pub async fn list_by_status(
        &self,
        status: &str,
        limit: i64,
        offset: i64,
    ) -> Result<Vec<JournalReadModel>, sqlx::Error> {
        let results = sqlx::query_as!(
            JournalReadModel,
            r#"
            SELECT
                journal_id,
                journal_date,
                description,
                fiscal_year,
                status,
                total_debit,
                total_credit,
                entry_count,
                approved_by,
                approved_at,
                created_at,
                updated_at
            FROM journal_read_model
            WHERE status = $1
            ORDER BY created_at DESC
            LIMIT $2 OFFSET $3
            "#,
            status,
            limit,
            offset
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(results)
    }

    /// 日付範囲で検索
    pub async fn find_by_date_range(
        &self,
        start_date: NaiveDate,
        end_date: NaiveDate,
    ) -> Result<Vec<JournalReadModel>, sqlx::Error> {
        let results = sqlx::query_as!(
            JournalReadModel,
            r#"
            SELECT
                journal_id,
                journal_date,
                description,
                fiscal_year,
                status,
                total_debit,
                total_credit,
                entry_count,
                approved_by,
                approved_at,
                created_at,
                updated_at
            FROM journal_read_model
            WHERE journal_date BETWEEN $1 AND $2
            ORDER BY journal_date DESC
            "#,
            start_date,
            end_date
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(results)
    }

    /// 明細を追加
    pub async fn add_entry(&self, entry: &JournalEntryReadModel) -> Result<i64, sqlx::Error> {
        let rec = sqlx::query!(
            r#"
            INSERT INTO journal_entry_read_model (
                journal_id, account_code, account_name,
                debit_amount, credit_amount, description, line_number
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7)
            RETURNING entry_id
            "#,
            entry.journal_id,
            entry.account_code,
            entry.account_name,
            entry.debit_amount,
            entry.credit_amount,
            entry.description,
            entry.line_number
        )
        .fetch_one(&self.pool)
        .await?;

        Ok(rec.entry_id)
    }

    /// 仕訳の明細を取得
    pub async fn get_entries(
        &self,
        journal_id: &str,
    ) -> Result<Vec<JournalEntryReadModel>, sqlx::Error> {
        let results = sqlx::query_as!(
            JournalEntryReadModel,
            r#"
            SELECT
                entry_id,
                journal_id,
                account_code,
                account_name,
                debit_amount,
                credit_amount,
                description,
                line_number,
                created_at
            FROM journal_entry_read_model
            WHERE journal_id = $1
            ORDER BY line_number ASC
            "#,
            journal_id
        )
        .fetch_all(&self.pool)
        .await?;

        Ok(results)
    }

    /// 仕訳の明細を削除
    pub async fn delete_entry(&self, entry_id: i64) -> Result<(), sqlx::Error> {
        sqlx::query!(
            r#"
            DELETE FROM journal_entry_read_model
            WHERE entry_id = $1
            "#,
            entry_id
        )
        .execute(&self.pool)
        .await?;

        Ok(())
    }
}

/// Decimal を i64 に変換（金額を整数で扱う）
pub fn decimal_to_i64(decimal: Decimal) -> i64 {
    (decimal * Decimal::from(100))
        .to_string()
        .parse()
        .unwrap_or(0)
}

/// i64 を Decimal に変換
pub fn i64_to_decimal(value: i64) -> Decimal {
    Decimal::from(value) / Decimal::from(100)
}
