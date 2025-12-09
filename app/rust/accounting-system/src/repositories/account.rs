use crate::domain::account::Account;
use rust_decimal::Decimal;
use sqlx::{PgPool, Result};

/// 勘定科目を登録
pub async fn insert(pool: &PgPool, account: &Account) -> Result<i32> {
    let row = sqlx::query_unchecked!(
        r#"
        INSERT INTO "勘定科目マスタ" (
            "勘定科目コード",
            "勘定科目名",
            "勘定科目カナ",
            "勘定科目種別",
            "合計科目",
            "BSPL区分",
            "取引要素区分",
            "費用区分",
            "表示順序",
            "集計対象",
            "課税取引コード",
            "残高"
        ) VALUES (
            $1, $2, $3, $4::account_type, $5, $6, $7, $8, $9, $10, $11, $12
        )
        RETURNING "勘定科目ID"
        "#,
        account.account_code,
        account.account_name,
        account.account_name_kana,
        account.account_type,
        account.is_summary_account,
        account.bspl_type,
        account.transaction_element_type,
        account.expense_type,
        account.display_order,
        account.is_aggregation_target,
        account.tax_code,
        account.balance
    )
    .fetch_one(pool)
    .await?;

    Ok(row.勘定科目ID)
}

/// 勘定科目コードで検索
pub async fn find_by_code(pool: &PgPool, account_code: &str) -> Result<Option<Account>> {
    let result = sqlx::query_as::<_, Account>(
        r#"
        SELECT "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
               "勘定科目種別"::TEXT as "勘定科目種別", "合計科目", "BSPL区分",
               "取引要素区分", "費用区分", "表示順序", "集計対象",
               "課税取引コード", "残高", "作成日時", "更新日時"
        FROM "勘定科目マスタ"
        WHERE "勘定科目コード" = $1
        "#,
    )
    .bind(account_code)
    .fetch_optional(pool)
    .await?;
    Ok(result)
}

/// 勘定科目IDで検索
pub async fn find_by_id(pool: &PgPool, account_id: i32) -> Result<Option<Account>> {
    let result = sqlx::query_as::<_, Account>(
        r#"
        SELECT "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
               "勘定科目種別"::TEXT as "勘定科目種別", "合計科目", "BSPL区分",
               "取引要素区分", "費用区分", "表示順序", "集計対象",
               "課税取引コード", "残高", "作成日時", "更新日時"
        FROM "勘定科目マスタ"
        WHERE "勘定科目ID" = $1
        "#,
    )
    .bind(account_id)
    .fetch_optional(pool)
    .await?;
    Ok(result)
}

/// 全ての勘定科目を取得
pub async fn find_all(pool: &PgPool) -> Result<Vec<Account>> {
    let results = sqlx::query_as::<_, Account>(
        r#"
        SELECT "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
               "勘定科目種別"::TEXT as "勘定科目種別", "合計科目", "BSPL区分",
               "取引要素区分", "費用区分", "表示順序", "集計対象",
               "課税取引コード", "残高", "作成日時", "更新日時"
        FROM "勘定科目マスタ"
        ORDER BY "勘定科目コード"
        "#,
    )
    .fetch_all(pool)
    .await?;
    Ok(results)
}

/// 勘定科目種別で検索
pub async fn find_by_type(pool: &PgPool, account_type: &str) -> Result<Vec<Account>> {
    let results = sqlx::query_as::<_, Account>(
        r#"
        SELECT "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
               "勘定科目種別"::TEXT as "勘定科目種別", "合計科目", "BSPL区分",
               "取引要素区分", "費用区分", "表示順序", "集計対象",
               "課税取引コード", "残高", "作成日時", "更新日時"
        FROM "勘定科目マスタ"
        WHERE "勘定科目種別"::TEXT = $1
        ORDER BY "表示順序", "勘定科目コード"
        "#,
    )
    .bind(account_type)
    .fetch_all(pool)
    .await?;
    Ok(results)
}

/// 合計科目を取得
pub async fn find_summary_accounts(pool: &PgPool) -> Result<Vec<Account>> {
    let results = sqlx::query_as::<_, Account>(
        r#"
        SELECT "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
               "勘定科目種別"::TEXT as "勘定科目種別", "合計科目", "BSPL区分",
               "取引要素区分", "費用区分", "表示順序", "集計対象",
               "課税取引コード", "残高", "作成日時", "更新日時"
        FROM "勘定科目マスタ"
        WHERE "合計科目" = true
        ORDER BY "表示順序", "勘定科目コード"
        "#,
    )
    .fetch_all(pool)
    .await?;
    Ok(results)
}

/// 明細科目を取得
pub async fn find_detail_accounts(pool: &PgPool) -> Result<Vec<Account>> {
    let results = sqlx::query_as::<_, Account>(
        r#"
        SELECT "勘定科目ID", "勘定科目コード", "勘定科目名", "勘定科目カナ",
               "勘定科目種別"::TEXT as "勘定科目種別", "合計科目", "BSPL区分",
               "取引要素区分", "費用区分", "表示順序", "集計対象",
               "課税取引コード", "残高", "作成日時", "更新日時"
        FROM "勘定科目マスタ"
        WHERE "合計科目" = false
        ORDER BY "表示順序", "勘定科目コード"
        "#,
    )
    .fetch_all(pool)
    .await?;
    Ok(results)
}

/// 勘定科目を更新
pub async fn update(pool: &PgPool, account: &Account) -> Result<u64> {
    let result = sqlx::query_unchecked!(
        r#"
        UPDATE "勘定科目マスタ"
        SET "勘定科目名" = $1,
            "勘定科目カナ" = $2,
            "勘定科目種別" = $3::account_type,
            "合計科目" = $4,
            "BSPL区分" = $5,
            "取引要素区分" = $6,
            "費用区分" = $7,
            "表示順序" = $8,
            "集計対象" = $9,
            "課税取引コード" = $10,
            "残高" = $11,
            "更新日時" = CURRENT_TIMESTAMP
        WHERE "勘定科目コード" = $12
        "#,
        account.account_name,
        account.account_name_kana,
        account.account_type,
        account.is_summary_account,
        account.bspl_type,
        account.transaction_element_type,
        account.expense_type,
        account.display_order,
        account.is_aggregation_target,
        account.tax_code,
        account.balance,
        account.account_code
    )
    .execute(pool)
    .await?;

    Ok(result.rows_affected())
}

/// 残高を更新
pub async fn update_balance(pool: &PgPool, account_code: &str, balance: Decimal) -> Result<u64> {
    let result = sqlx::query!(
        r#"
        UPDATE "勘定科目マスタ"
        SET "残高" = $1,
            "更新日時" = CURRENT_TIMESTAMP
        WHERE "勘定科目コード" = $2
        "#,
        balance,
        account_code
    )
    .execute(pool)
    .await?;

    Ok(result.rows_affected())
}

/// 勘定科目を削除
pub async fn delete(pool: &PgPool, account_code: &str) -> Result<u64> {
    let result = sqlx::query!(
        r#"
        DELETE FROM "勘定科目マスタ"
        WHERE "勘定科目コード" = $1
        "#,
        account_code
    )
    .execute(pool)
    .await?;

    Ok(result.rows_affected())
}
