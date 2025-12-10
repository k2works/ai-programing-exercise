use chrono::NaiveDate;
use rust_decimal_macros::dec;
use sqlx::PgPool;

use crate::common::*;
use accounting_system::application::financial::financial_statement_service::FinancialStatementService;
use accounting_system::domain::financial::financial_report::FinancialReport;

#[tokio::test]
async fn test_create_financial_report() {
    // Given: テストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_financial_report_test_data(pool).await;

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

    // 財務指標を計算
    let financial_ratios = service.calculate_financial_ratios(&balance_sheet, &income_statement);

    // When: 財務分析レポートを作成
    let report = FinancialReport {
        report_date: NaiveDate::from_ymd_opt(2024, 2, 1).unwrap(),
        balance_sheet: balance_sheet.clone(),
        income_statement: income_statement.clone(),
        financial_ratios: financial_ratios.clone(),
        analysis_comments: Vec::new(),
    };

    // Then: レポートが正しく作成されている
    assert_eq!(
        report.report_date,
        NaiveDate::from_ymd_opt(2024, 2, 1).unwrap()
    );
    assert_eq!(report.balance_sheet.total_assets, dec!(10000000));
    assert_eq!(report.income_statement.total_sales, dec!(10000000));
    assert_eq!(report.financial_ratios.equity_ratio, dec!(50.00));
}

#[tokio::test]
async fn test_generate_analysis_comments() {
    // Given: テストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_financial_report_test_data(pool).await;

    let service = FinancialStatementService::new(pool.clone());
    let as_of_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let balance_sheet = service.generate_balance_sheet(as_of_date).await.unwrap();

    let start_date = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap();
    let end_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let income_statement = service
        .generate_income_statement(start_date, end_date)
        .await
        .unwrap();

    let financial_ratios = service.calculate_financial_ratios(&balance_sheet, &income_statement);

    // When: 財務分析レポートを作成し、コメントを生成
    let mut report = FinancialReport {
        report_date: NaiveDate::from_ymd_opt(2024, 2, 1).unwrap(),
        balance_sheet,
        income_statement,
        financial_ratios,
        analysis_comments: Vec::new(),
    };

    let comments = report.generate_analysis_comments();
    report.analysis_comments = comments;

    // Then: 分析コメントが生成されている
    assert!(!report.analysis_comments.is_empty());
    assert!(report.analysis_comments.len() >= 3); // 安全性2つ + 収益性1つ以上

    // 流動比率のコメント（1600%）
    assert!(report
        .analysis_comments
        .iter()
        .any(|c| c.contains("流動比率") && c.contains("良好")));

    // 自己資本比率のコメント（50%）
    assert!(report
        .analysis_comments
        .iter()
        .any(|c| c.contains("自己資本比率") && c.contains("健全")));

    // 売上高純利益率のコメント（20%）
    assert!(report
        .analysis_comments
        .iter()
        .any(|c| c.contains("売上高純利益率") && c.contains("良好")));
}

#[tokio::test]
async fn test_analysis_comments_warning() {
    // Given: 悪い財務状況のテストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_poor_financial_test_data(pool).await;

    let service = FinancialStatementService::new(pool.clone());
    let as_of_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let balance_sheet = service.generate_balance_sheet(as_of_date).await.unwrap();

    let start_date = NaiveDate::from_ymd_opt(2024, 1, 1).unwrap();
    let end_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let income_statement = service
        .generate_income_statement(start_date, end_date)
        .await
        .unwrap();

    let financial_ratios = service.calculate_financial_ratios(&balance_sheet, &income_statement);

    // When: 財務分析レポートを作成
    let mut report = FinancialReport {
        report_date: NaiveDate::from_ymd_opt(2024, 2, 1).unwrap(),
        balance_sheet,
        income_statement,
        financial_ratios,
        analysis_comments: Vec::new(),
    };

    let comments = report.generate_analysis_comments();
    report.analysis_comments = comments;

    // Then: 警告コメントが生成されている
    assert!(!report.analysis_comments.is_empty());

    // 流動比率の警告（50%）
    assert!(report
        .analysis_comments
        .iter()
        .any(|c| c.contains("流動比率") && c.contains("注意")));

    // 自己資本比率の警告（10%）
    assert!(report
        .analysis_comments
        .iter()
        .any(|c| c.contains("自己資本比率") && c.contains("低水準")));

    // 売上高純利益率の警告（2%）
    assert!(report
        .analysis_comments
        .iter()
        .any(|c| c.contains("売上高純利益率") && c.contains("低水準")));
}

async fn setup_financial_report_test_data(pool: &PgPool) {
    // テーブルをクリア
    sqlx::query(r#"TRUNCATE TABLE "日次勘定科目残高" CASCADE"#)
        .execute(pool)
        .await
        .unwrap();
    sqlx::query(r#"TRUNCATE TABLE "勘定科目マスタ" CASCADE"#)
        .execute(pool)
        .await
        .unwrap();

    // 貸借対照表科目
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

    // 損益計算書科目
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

    // 貸借対照表の残高
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

    // 損益計算書の残高（3回分）
    for (date, sales, selling_expense) in [
        (
            NaiveDate::from_ymd_opt(2024, 1, 10).unwrap(),
            3333333,
            666666,
        ),
        (
            NaiveDate::from_ymd_opt(2024, 1, 20).unwrap(),
            3333333,
            666667,
        ),
        (
            NaiveDate::from_ymd_opt(2024, 1, 31).unwrap(),
            3333334,
            666667,
        ),
    ] {
        sqlx::query(
            r#"
            INSERT INTO "日次勘定科目残高" (
                "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
            ) VALUES
            ($1, '4110', '', '', '', 0, 0, $2),
            ($1, '5110', '', '', '', 0, 2000000, 0),
            ($1, '5210', '', '', '', 0, $3, 0)
            "#,
        )
        .bind(date)
        .bind(sales)
        .bind(selling_expense)
        .execute(pool)
        .await
        .unwrap();
    }
}

async fn setup_poor_financial_test_data(pool: &PgPool) {
    // テーブルをクリア
    sqlx::query(r#"TRUNCATE TABLE "日次勘定科目残高" CASCADE"#)
        .execute(pool)
        .await
        .unwrap();
    sqlx::query(r#"TRUNCATE TABLE "勘定科目マスタ" CASCADE"#)
        .execute(pool)
        .await
        .unwrap();

    // 貸借対照表科目
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

    // 損益計算書科目
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

    // 悪い財務状況の貸借対照表（流動比率50%、自己資本比率10%）
    let bs_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    sqlx::query(
        r#"
        INSERT INTO "日次勘定科目残高" (
            "起票日", "勘定科目コード", "補助科目コード", "部門コード",
            "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
        ) VALUES
        ($1, '1110', '', '', '', 0, 1000000, 0),
        ($1, '1410', '', '', '', 0, 9000000, 0),
        ($1, '2110', '', '', '', 0, 0, 2000000),
        ($1, '2510', '', '', '', 0, 0, 7000000),
        ($1, '3110', '', '', '', 0, 0, 1000000)
        "#,
    )
    .bind(bs_date)
    .execute(pool)
    .await
    .unwrap();

    // 悪い損益計算書（売上高純利益率2%）
    for date in [
        NaiveDate::from_ymd_opt(2024, 1, 10).unwrap(),
        NaiveDate::from_ymd_opt(2024, 1, 20).unwrap(),
        NaiveDate::from_ymd_opt(2024, 1, 31).unwrap(),
    ] {
        sqlx::query(
            r#"
            INSERT INTO "日次勘定科目残高" (
                "起票日", "勘定科目コード", "補助科目コード", "部門コード",
                "プロジェクトコード", "決算仕訳フラグ", "借方金額", "貸方金額"
            ) VALUES
            ($1, '4110', '', '', '', 0, 0, 3333333),
            ($1, '5110', '', '', '', 0, 2900000, 0),
            ($1, '5210', '', '', '', 0, 366666, 0)
            "#,
        )
        .bind(date)
        .execute(pool)
        .await
        .unwrap();
    }
}
