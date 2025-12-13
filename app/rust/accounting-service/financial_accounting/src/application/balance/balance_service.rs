use chrono::NaiveDate;
use rust_decimal::Decimal;
use sqlx::{PgPool, Postgres, Transaction};

/// 残高管理サービス
pub struct BalanceService {
    pool: PgPool,
}

impl BalanceService {
    /// コンストラクタ
    pub fn new(pool: PgPool) -> Self {
        Self { pool }
    }

    /// 日次残高を更新（UPSERT）
    ///
    /// PostgreSQL の ON CONFLICT ... DO UPDATE を使用して、
    /// 同一キーが存在する場合は残高を累積し、存在しない場合は新規登録します。
    ///
    /// # Arguments
    ///
    /// * `entry_date` - 起票日
    /// * `account_code` - 勘定科目コード
    /// * `sub_account_code` - 補助科目コード（None の場合は空文字列）
    /// * `department_code` - 部門コード（None の場合は空文字列）
    /// * `project_code` - プロジェクトコード（None の場合は空文字列）
    /// * `settlement_flag` - 決算仕訳フラグ（None の場合は 0）
    /// * `debit_amount` - 借方金額
    /// * `credit_amount` - 貸方金額
    #[allow(clippy::too_many_arguments)]
    pub async fn update_daily_balance(
        &self,
        entry_date: NaiveDate,
        account_code: &str,
        sub_account_code: Option<&str>,
        department_code: Option<&str>,
        project_code: Option<&str>,
        settlement_flag: Option<i32>,
        debit_amount: Decimal,
        credit_amount: Decimal,
    ) -> Result<(), sqlx::Error> {
        let sql = r#"
            INSERT INTO "日次勘定科目残高" (
                "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
            ON CONFLICT ("起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ")
            DO UPDATE SET
                "借方金額" = "日次勘定科目残高"."借方金額" + EXCLUDED."借方金額",
                "貸方金額" = "日次勘定科目残高"."貸方金額" + EXCLUDED."貸方金額",
                "更新日時" = CURRENT_TIMESTAMP
        "#;

        sqlx::query(sql)
            .bind(entry_date)
            .bind(account_code)
            .bind(sub_account_code.unwrap_or(""))
            .bind(department_code.unwrap_or(""))
            .bind(project_code.unwrap_or(""))
            .bind(settlement_flag.unwrap_or(0))
            .bind(debit_amount)
            .bind(credit_amount)
            .execute(&self.pool)
            .await?;

        Ok(())
    }

    /// 仕訳貸借明細から日次残高を一括更新
    ///
    /// 指定された仕訳番号の仕訳貸借明細を集計して、
    /// 日次勘定科目残高テーブルを一括で UPSERT します。
    ///
    /// # Arguments
    ///
    /// * `journal_no` - 仕訳伝票番号
    pub async fn update_balance_from_journal_items(
        &self,
        journal_no: &str,
    ) -> Result<(), sqlx::Error> {
        let sql = r#"
            INSERT INTO "日次勘定科目残高" (
                "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
            )
            SELECT
                j."起票日",
                item."勘定科目コード",
                COALESCE(item."補助科目コード", ''),
                COALESCE(item."部門コード", ''),
                COALESCE(item."プロジェクトコード", ''),
                j."決算仕訳フラグ",
                SUM(CASE WHEN item."仕訳行貸借区分" = 'D' THEN item."仕訳金額" ELSE 0 END),
                SUM(CASE WHEN item."仕訳行貸借区分" = 'C' THEN item."仕訳金額" ELSE 0 END)
            FROM "仕訳貸借明細" item
            INNER JOIN "仕訳明細" detail
                ON item."仕訳伝票番号" = detail."仕訳伝票番号"
                AND item."仕訳行番号" = detail."仕訳行番号"
            INNER JOIN "仕訳" j
                ON detail."仕訳伝票番号" = j."仕訳伝票番号"
            WHERE j."仕訳伝票番号" = $1
            GROUP BY
                j."起票日",
                item."勘定科目コード",
                COALESCE(item."補助科目コード", ''),
                COALESCE(item."部門コード", ''),
                COALESCE(item."プロジェクトコード", ''),
                j."決算仕訳フラグ"
            ON CONFLICT ("起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ")
            DO UPDATE SET
                "借方金額" = "日次勘定科目残高"."借方金額" + EXCLUDED."借方金額",
                "貸方金額" = "日次勘定科目残高"."貸方金額" + EXCLUDED."貸方金額",
                "更新日時" = CURRENT_TIMESTAMP
        "#;

        sqlx::query(sql)
            .bind(journal_no)
            .execute(&self.pool)
            .await?;

        Ok(())
    }

    /// トランザクション内で日次残高を更新
    ///
    /// すでに開始されたトランザクション内で残高を更新します。
    /// 仕訳登録とトランザクション境界を共有する場合に使用します。
    ///
    /// # Arguments
    ///
    /// * `tx` - PostgreSQL トランザクション
    /// * `entry_date` - 起票日
    /// * `account_code` - 勘定科目コード
    /// * `sub_account_code` - 補助科目コード
    /// * `department_code` - 部門コード
    /// * `project_code` - プロジェクトコード
    /// * `settlement_flag` - 決算仕訳フラグ
    /// * `debit_amount` - 借方金額
    /// * `credit_amount` - 貸方金額
    #[allow(clippy::too_many_arguments)]
    pub async fn update_daily_balance_in_tx(
        tx: &mut Transaction<'_, Postgres>,
        entry_date: NaiveDate,
        account_code: &str,
        sub_account_code: Option<&str>,
        department_code: Option<&str>,
        project_code: Option<&str>,
        settlement_flag: Option<i32>,
        debit_amount: Decimal,
        credit_amount: Decimal,
    ) -> Result<(), sqlx::Error> {
        let sql = r#"
            INSERT INTO "日次勘定科目残高" (
                "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8)
            ON CONFLICT ("起票日", "勘定科目コード", "補助科目コード", "部門コード", "プロジェクトコード", "決算仕訳フラグ")
            DO UPDATE SET
                "借方金額" = "日次勘定科目残高"."借方金額" + EXCLUDED."借方金額",
                "貸方金額" = "日次勘定科目残高"."貸方金額" + EXCLUDED."貸方金額",
                "更新日時" = CURRENT_TIMESTAMP
        "#;

        sqlx::query(sql)
            .bind(entry_date)
            .bind(account_code)
            .bind(sub_account_code.unwrap_or(""))
            .bind(department_code.unwrap_or(""))
            .bind(project_code.unwrap_or(""))
            .bind(settlement_flag.unwrap_or(0))
            .bind(debit_amount)
            .bind(credit_amount)
            .execute(&mut **tx)
            .await?;

        Ok(())
    }
}
