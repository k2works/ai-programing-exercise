use async_trait::async_trait;
use sqlx::PgPool;

use crate::application::ports::output::journal_repository::JournalRepository;
use crate::domain::journal::{Journal, JournalDebitCreditItem, JournalDetail};

/// 仕訳リポジトリの実装（Output Adapter）
pub struct JournalRepositoryImpl {
    pool: PgPool,
}

impl JournalRepositoryImpl {
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }
}

#[async_trait]
impl JournalRepository for JournalRepositoryImpl {
    async fn create(&self, journal: Journal) -> Result<Journal, Box<dyn std::error::Error>> {
        let mut tx = self.pool.begin().await?;

        // 1. 仕訳ヘッダーを保存
        sqlx::query(
            r#"
            INSERT INTO "仕訳" (
                "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
                "仕訳伝票区分", "定期計上フラグ", "社員コード", "部門コード", "赤伝フラグ", "赤黒伝票番号"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11)
            "#,
        )
        .bind(&journal.journal_no)
        .bind(journal.journal_date)
        .bind(journal.input_date)
        .bind(journal.settlement_flag)
        .bind(journal.single_entry_flag)
        .bind(journal.journal_type)
        .bind(journal.recurring_flag)
        .bind(&journal.employee_code)
        .bind(&journal.department_code)
        .bind(journal.reversal_flag)
        .bind(&journal.reversal_journal_no)
        .execute(&mut *tx)
        .await?;

        // 2. 仕訳明細を保存
        for detail in &journal.details {
            sqlx::query(
                r#"
                INSERT INTO "仕訳明細" (
                    "仕訳伝票番号", "仕訳行番号", "行摘要"
                ) VALUES ($1, $2, $3)
                "#,
            )
            .bind(&detail.journal_no)
            .bind(detail.line_number)
            .bind(&detail.description)
            .execute(&mut *tx)
            .await?;

            // 3. 仕訳貸借明細を保存
            for item in &detail.items {
                sqlx::query(
                    r#"
                    INSERT INTO "仕訳貸借明細" (
                        "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
                        "通貨コード", "為替レート", "部門コード", "プロジェクトコード",
                        "勘定科目コード", "補助科目コード", "仕訳金額", "基軸換算仕訳金額",
                        "消費税区分", "消費税率", "消費税計算区分", "期日", "資金繰フラグ",
                        "セグメントコード", "相手勘定科目コード", "相手補助科目コード",
                        "付箋コード", "付箋内容"
                    ) VALUES (
                        $1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11,
                        $12, $13, $14, $15, $16, $17, $18, $19, $20, $21
                    )
                    "#,
                )
                .bind(&item.journal_no)
                .bind(item.line_number)
                .bind(&item.debit_credit_flag)
                .bind(&item.currency_code)
                .bind(item.exchange_rate)
                .bind(&item.department_code)
                .bind(&item.project_code)
                .bind(&item.account_code)
                .bind(&item.sub_account_code)
                .bind(item.amount)
                .bind(item.base_amount)
                .bind(&item.tax_division)
                .bind(item.tax_rate)
                .bind(&item.tax_calculation_type)
                .bind(item.due_date)
                .bind(item.cash_flow_flag)
                .bind(&item.segment_code)
                .bind(&item.contra_account_code)
                .bind(&item.contra_sub_account_code)
                .bind(&item.note_code)
                .bind(&item.note_content)
                .execute(&mut *tx)
                .await?;
            }
        }

        // トランザクションをコミット
        tx.commit().await?;

        Ok(journal)
    }

    async fn find_by_no(
        &self,
        journal_no: &str,
    ) -> Result<Option<Journal>, Box<dyn std::error::Error>> {
        // 1. 仕訳ヘッダーを取得
        let journal_row = sqlx::query!(
            r#"
            SELECT
                "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
                "仕訳伝票区分", "定期計上フラグ", "社員コード", "部門コード", "赤伝フラグ", "赤黒伝票番号"
            FROM "仕訳"
            WHERE "仕訳伝票番号" = $1
            "#,
            journal_no
        )
        .fetch_optional(&self.pool)
        .await?;

        if journal_row.is_none() {
            return Ok(None);
        }

        let journal_row = journal_row.unwrap();

        // 2. 仕訳明細を取得
        let detail_rows = sqlx::query!(
            r#"
            SELECT "仕訳伝票番号", "仕訳行番号", "行摘要"
            FROM "仕訳明細"
            WHERE "仕訳伝票番号" = $1
            ORDER BY "仕訳行番号"
            "#,
            journal_no
        )
        .fetch_all(&self.pool)
        .await?;

        let mut details = Vec::new();

        for detail_row in detail_rows {
            // 3. 仕訳貸借明細を取得
            let item_rows = sqlx::query!(
                r#"
                SELECT
                    "仕訳伝票番号", "仕訳行番号", "仕訳行貸借区分",
                    "通貨コード", "為替レート", "部門コード", "プロジェクトコード",
                    "勘定科目コード", "補助科目コード", "仕訳金額", "基軸換算仕訳金額",
                    "消費税区分", "消費税率", "消費税計算区分", "期日", "資金繰フラグ",
                    "セグメントコード", "相手勘定科目コード", "相手補助科目コード",
                    "付箋コード", "付箋内容"
                FROM "仕訳貸借明細"
                WHERE "仕訳伝票番号" = $1 AND "仕訳行番号" = $2
                ORDER BY "仕訳行貸借区分"
                "#,
                detail_row.仕訳伝票番号,
                detail_row.仕訳行番号
            )
            .fetch_all(&self.pool)
            .await?;

            let mut items = Vec::new();
            for item_row in item_rows {
                items.push(JournalDebitCreditItem {
                    journal_no: item_row.仕訳伝票番号,
                    line_number: item_row.仕訳行番号,
                    debit_credit_flag: item_row.仕訳行貸借区分,
                    currency_code: item_row.通貨コード,
                    exchange_rate: item_row.為替レート,
                    department_code: item_row.部門コード,
                    project_code: item_row.プロジェクトコード,
                    account_code: item_row.勘定科目コード,
                    sub_account_code: item_row.補助科目コード,
                    amount: item_row.仕訳金額,
                    base_amount: item_row.基軸換算仕訳金額,
                    tax_division: item_row.消費税区分,
                    tax_rate: item_row.消費税率,
                    tax_calculation_type: item_row.消費税計算区分,
                    due_date: item_row.期日,
                    cash_flow_flag: item_row.資金繰フラグ,
                    segment_code: item_row.セグメントコード,
                    contra_account_code: item_row.相手勘定科目コード,
                    contra_sub_account_code: item_row.相手補助科目コード,
                    note_code: item_row.付箋コード,
                    note_content: item_row.付箋内容,
                });
            }

            details.push(JournalDetail {
                journal_no: detail_row.仕訳伝票番号,
                line_number: detail_row.仕訳行番号,
                description: detail_row.行摘要,
                items,
            });
        }

        let journal = Journal {
            journal_no: journal_row.仕訳伝票番号,
            journal_date: journal_row.起票日,
            input_date: journal_row.入力日,
            settlement_flag: journal_row.決算仕訳フラグ,
            single_entry_flag: journal_row.単振フラグ,
            journal_type: journal_row.仕訳伝票区分,
            recurring_flag: journal_row.定期計上フラグ,
            employee_code: journal_row.社員コード,
            department_code: journal_row.部門コード,
            reversal_flag: journal_row.赤伝フラグ,
            reversal_journal_no: journal_row.赤黒伝票番号,
            details,
        };

        Ok(Some(journal))
    }

    async fn find_all(&self) -> Result<Vec<Journal>, Box<dyn std::error::Error>> {
        let journal_rows = sqlx::query!(
            r#"
            SELECT
                "仕訳伝票番号", "起票日", "入力日", "決算仕訳フラグ", "単振フラグ",
                "仕訳伝票区分", "定期計上フラグ", "社員コード", "部門コード", "赤伝フラグ", "赤黒伝票番号"
            FROM "仕訳"
            ORDER BY "起票日", "仕訳伝票番号"
            "#
        )
        .fetch_all(&self.pool)
        .await?;

        let mut journals = Vec::new();

        for journal_row in journal_rows {
            if let Some(journal) = self.find_by_no(&journal_row.仕訳伝票番号).await? {
                journals.push(journal);
            }
        }

        Ok(journals)
    }
}
