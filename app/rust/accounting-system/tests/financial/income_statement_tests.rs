use chrono::NaiveDate;
use rust_decimal_macros::dec;
use sqlx::PgPool;

use crate::common::*;
use accounting_system::application::financial::financial_statement_service::FinancialStatementService;

#[tokio::test]
async fn test_generate_income_statement() {
    // Given: テストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_income_statement_test_data(pool).await;

    // When: 2024年1月の損益計算書を生成
    let start_date = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap();
    let end_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let service = FinancialStatementService::new(pool.clone());
    let income_statement = service
        .generate_income_statement(start_date, end_date)
        .await
        .unwrap();

    // Then: 損益計算書のプロパティが存在する
    assert_eq!(income_statement.start_date, start_date);
    assert_eq!(income_statement.end_date, end_date);
    assert!(!income_statement.revenues.is_empty());
    assert!(!income_statement.expenses.is_empty());
    assert!(income_statement.total_sales > dec!(0));
}

#[tokio::test]
async fn test_profit_calculation() {
    // Given: テストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_income_statement_test_data(pool).await;

    // When: 損益計算書を生成
    let start_date = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap();
    let end_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let service = FinancialStatementService::new(pool.clone());
    let income_statement = service
        .generate_income_statement(start_date, end_date)
        .await
        .unwrap();

    // Then: 利益項目が正しく計算されている
    // 売上高 = 10,000,000
    assert_eq!(income_statement.total_sales, dec!(10000000));

    // 売上原価 = 6,000,000
    assert_eq!(income_statement.cost_of_sales, dec!(6000000));

    // 売上総利益 = 売上高 - 売上原価 = 4,000,000
    assert_eq!(income_statement.gross_profit, dec!(4000000));

    // 販管費 = 2,000,000
    assert_eq!(income_statement.operating_expenses, dec!(2000000));

    // 営業利益 = 売上総利益 - 販管費 = 2,000,000
    assert_eq!(income_statement.operating_income, dec!(2000000));

    // 当期純利益 = 2,000,000（簡略化）
    assert_eq!(income_statement.net_income, dec!(2000000));
}

#[tokio::test]
async fn test_revenue_expense_classification() {
    // Given: テストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_income_statement_test_data(pool).await;

    // When: 損益計算書を生成
    let start_date = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap();
    let end_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let service = FinancialStatementService::new(pool.clone());
    let income_statement = service
        .generate_income_statement(start_date, end_date)
        .await
        .unwrap();

    // Then: 収益項目が正しく分類されている
    let revenues = &income_statement.revenues;
    assert_eq!(revenues.len(), 1);
    assert_eq!(revenues[0].account_code, "4110");
    assert_eq!(revenues[0].account_name, "売上高");
    assert_eq!(revenues[0].amount, dec!(10000000));

    // Then: 費用項目が正しく分類されている
    let expenses = &income_statement.expenses;
    assert_eq!(expenses.len(), 2);

    // 売上原価
    let cost_of_sales = expenses.iter().find(|e| e.account_code == "5110").unwrap();
    assert_eq!(cost_of_sales.account_name, "売上原価");
    assert_eq!(cost_of_sales.amount, dec!(6000000));

    // 販管費
    let selling_expenses = expenses.iter().find(|e| e.account_code == "5210").unwrap();
    assert_eq!(selling_expenses.account_name, "販売費");
    assert_eq!(selling_expenses.amount, dec!(2000000));
}

#[tokio::test]
async fn test_income_statement_percentage() {
    // Given: テストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_income_statement_test_data(pool).await;

    // When: 損益計算書を生成
    let start_date = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap();
    let end_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let service = FinancialStatementService::new(pool.clone());
    let income_statement = service
        .generate_income_statement(start_date, end_date)
        .await
        .unwrap();

    // Then: 構成比率が計算されている
    for revenue in &income_statement.revenues {
        assert!(revenue.percentage >= dec!(0));
        assert!(revenue.percentage <= dec!(100));
    }

    // 売上高の構成比率 = 100%
    let sales = income_statement
        .revenues
        .iter()
        .find(|r| r.account_code == "4110")
        .unwrap();
    assert_eq!(sales.percentage, dec!(100.00));

    // 売上原価の構成比率 = 6,000,000 / 10,000,000 × 100 = 60%
    let cost = income_statement
        .expenses
        .iter()
        .find(|e| e.account_code == "5110")
        .unwrap();
    assert_eq!(cost.percentage, dec!(60.00));
}

async fn setup_income_statement_test_data(pool: &PgPool) {
    // テーブルをクリア
    sqlx::query(r#"TRUNCATE TABLE "日次勘定科目残高" CASCADE"#)
        .execute(pool)
        .await
        .unwrap();
    sqlx::query(r#"TRUNCATE TABLE "勘定科目マスタ" CASCADE"#)
        .execute(pool)
        .await
        .unwrap();

    // テストデータ：勘定科目マスタ（損益計算書科目）
    // BSPL区分='P'（損益計算書）
    // 費用区分: 1=売上原価, 2=販管費
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ" (
            "勘定科目コード", "勘定科目名", "勘定科目カナ", "勘定科目種別",
            "BSPL区分", "貸借区分", "取引要素区分", "集計区分", "費用区分"
        ) VALUES
        ('4110', '売上高', 'ウリアゲダカ', '収益', 'P', 'C', '収益', 'D', '0'),
        ('5110', '売上原価', 'ウリアゲゲンカ', '費用', 'P', 'D', '費用', 'D', '1'),
        ('5210', '販売費', 'ハンバイヒ', '費用', 'P', 'D', '費用', 'D', '2')
        "#,
    )
    .execute(pool)
    .await
    .unwrap();

    // テストデータ：日次残高（2024年1月）
    // 売上高: 3,333,333 + 3,333,333 + 3,333,334 = 10,000,000
    // 売上原価: 2,000,000 + 2,000,000 + 2,000,000 = 6,000,000
    // 販売費: 666,666 + 666,667 + 666,667 = 2,000,000

    // 1回目
    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES
        ($1, '4110', '', '', '', 0, 0, 3333333),
        ($1, '5110', '', '', '', 0, 2000000, 0),
        ($1, '5210', '', '', '', 0, 666666, 0)
        "#,
    )
    .bind(NaiveDate::from_ymd_opt(2024, 1, 10).unwrap())
    .execute(pool)
    .await
    .unwrap();

    // 2回目
    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES
        ($1, '4110', '', '', '', 0, 0, 3333333),
        ($1, '5110', '', '', '', 0, 2000000, 0),
        ($1, '5210', '', '', '', 0, 666667, 0)
        "#,
    )
    .bind(NaiveDate::from_ymd_opt(2024, 1, 20).unwrap())
    .execute(pool)
    .await
    .unwrap();

    // 3回目（端数調整）
    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES
        ($1, '4110', '', '', '', 0, 0, 3333334),
        ($1, '5110', '', '', '', 0, 2000000, 0),
        ($1, '5210', '', '', '', 0, 666667, 0)
        "#,
    )
    .bind(NaiveDate::from_ymd_opt(2024, 1, 31).unwrap())
    .execute(pool)
    .await
    .unwrap();
}
