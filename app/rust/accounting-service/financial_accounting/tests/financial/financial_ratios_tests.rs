use chrono::NaiveDate;
use rust_decimal_macros::dec;
use sqlx::PgPool;

use crate::common::*;
use accounting_system::application::financial::financial_statement_service::FinancialStatementService;

#[tokio::test]
async fn test_calculate_financial_ratios() {
    // Given: 貸借対照表と損益計算書のテストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_financial_ratios_test_data(pool).await;

    let service = FinancialStatementService::new(pool.clone());

    // 貸借対照表を生成
    let as_of_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let balance_sheet = service.generate_balance_sheet(as_of_date).await.unwrap();

    // 損益計算書を生成
    let start_date = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap();
    let end_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let income_statement = service
        .generate_income_statement(start_date, end_date)
        .await
        .unwrap();

    // When: 財務指標を計算
    let ratios = service.calculate_financial_ratios(&balance_sheet, &income_statement);

    // Then: 財務指標が計算されている
    assert!(ratios.current_ratio > dec!(0));
    assert!(ratios.equity_ratio > dec!(0));
    assert!(ratios.gross_profit_margin > dec!(0));
    assert!(ratios.operating_profit_margin > dec!(0));
    assert!(ratios.net_profit_margin > dec!(0));
    assert!(ratios.roa > dec!(0));
    assert!(ratios.roe > dec!(0));
}

#[tokio::test]
async fn test_safety_ratios() {
    // Given: テストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_financial_ratios_test_data(pool).await;

    let service = FinancialStatementService::new(pool.clone());
    let as_of_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let balance_sheet = service.generate_balance_sheet(as_of_date).await.unwrap();

    let start_date = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap();
    let end_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let income_statement = service
        .generate_income_statement(start_date, end_date)
        .await
        .unwrap();

    // When: 財務指標を計算
    let ratios = service.calculate_financial_ratios(&balance_sheet, &income_statement);

    // Then: 安全性指標が正しく計算されている
    // 流動比率 = 流動資産(8,000,000) / 流動負債(500,000) × 100 = 1600%
    assert_eq!(ratios.current_ratio, dec!(1600.00));

    // 自己資本比率 = 自己資本(5,000,000) / 総資産(10,000,000) × 100 = 50%
    assert_eq!(ratios.equity_ratio, dec!(50.00));
}

#[tokio::test]
async fn test_profitability_ratios() {
    // Given: テストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_financial_ratios_test_data(pool).await;

    let service = FinancialStatementService::new(pool.clone());
    let as_of_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let balance_sheet = service.generate_balance_sheet(as_of_date).await.unwrap();

    let start_date = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap();
    let end_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let income_statement = service
        .generate_income_statement(start_date, end_date)
        .await
        .unwrap();

    // When: 財務指標を計算
    let ratios = service.calculate_financial_ratios(&balance_sheet, &income_statement);

    // Then: 収益性指標が正しく計算されている
    // 売上高総利益率 = 売上総利益(4,000,000) / 売上高(10,000,000) × 100 = 40%
    assert_eq!(ratios.gross_profit_margin, dec!(40.00));

    // 売上高営業利益率 = 営業利益(2,000,000) / 売上高(10,000,000) × 100 = 20%
    assert_eq!(ratios.operating_profit_margin, dec!(20.00));

    // 売上高純利益率 = 当期純利益(2,000,000) / 売上高(10,000,000) × 100 = 20%
    assert_eq!(ratios.net_profit_margin, dec!(20.00));

    // ROA = 当期純利益(2,000,000) / 総資産(10,000,000) × 100 = 20%
    assert_eq!(ratios.roa, dec!(20.00));

    // ROE = 当期純利益(2,000,000) / 自己資本(5,000,000) × 100 = 40%
    assert_eq!(ratios.roe, dec!(40.00));
}

async fn setup_financial_ratios_test_data(pool: &PgPool) {
    // テーブルをクリア
    sqlx::query(r#"TRUNCATE TABLE "日次勘定科目残高" CASCADE"#)
        .execute(pool)
        .await
        .unwrap();
    sqlx::query(r#"TRUNCATE TABLE "勘定科目マスタ" CASCADE"#)
        .execute(pool)
        .await
        .unwrap();

    // 貸借対照表科目（資産・負債・純資産）
    sqlx::query(
        r#"
        INSERT INTO "勘定科目マスタ" (
            "勘定科目コード", "勘定科目名", "勘定科目カナ", "勘定科目種別",
            "BSPL区分", "貸借区分", "取引要素区分", "集計区分"
        ) VALUES
        ('1110', '普通預金', 'フツウヨキン', '資産', 'B', 'D', '資産', 'D'),
        ('1410', '建物', 'タテモノ', '資産', 'B', 'D', '資産', 'D'),
        ('2110', '買掛金', 'カイカケキン', '負債', 'B', 'C', '負債', 'D'),
        ('2510', '長期借入金', 'チョウキカリイレキン', '負債', 'B', 'C', '負債', 'D'),
        ('3110', '資本金', 'シホンキン', '純資産', 'B', 'C', '資本', 'D')
        "#,
    )
    .execute(pool)
    .await
    .unwrap();

    // 損益計算書科目（収益・費用）
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

    // 貸借対照表の残高（2024-01-31時点）
    let bs_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES
        ($1, '1110', '', '', '', 0, 8000000, 0),
        ($1, '1410', '', '', '', 0, 2000000, 0),
        ($1, '2110', '', '', '', 0, 0, 500000),
        ($1, '2510', '', '', '', 0, 0, 4500000),
        ($1, '3110', '', '', '', 0, 0, 5000000)
        "#,
    )
    .bind(bs_date)
    .execute(pool)
    .await
    .unwrap();

    // 損益計算書の残高（2024年1月）
    // 売上高: 10,000,000
    // 売上原価: 6,000,000
    // 販売費: 2,000,000

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
