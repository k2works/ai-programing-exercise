use chrono::NaiveDate;
use rust_decimal_macros::dec;
use sqlx::PgPool;

use crate::common::*;
use accounting_system::application::financial::financial_statement_service::FinancialStatementService;

#[tokio::test]
async fn test_generate_balance_sheet() {
    // Given: テストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_balance_sheet_test_data(pool).await;

    // When: 2024-01-31時点の貸借対照表を生成
    let as_of_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let service = FinancialStatementService::new(pool.clone());
    let balance_sheet = service.generate_balance_sheet(as_of_date).await.unwrap();

    // Then: 貸借対照表のプロパティが存在する
    assert_eq!(balance_sheet.as_of_date, as_of_date);
    assert!(!balance_sheet.assets.is_empty());
    assert!(!balance_sheet.liabilities.is_empty());
    assert!(!balance_sheet.equity.is_empty());
    assert!(balance_sheet.total_assets > dec!(0));
    assert!(balance_sheet.total_liabilities > dec!(0));
    assert!(balance_sheet.total_equity > dec!(0));
    assert!(balance_sheet.total_liabilities_and_equity > dec!(0));
}

#[tokio::test]
async fn test_balance_sheet_accounting_equation() {
    // Given: テストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_balance_sheet_test_data(pool).await;

    // When: 貸借対照表を生成
    let as_of_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let service = FinancialStatementService::new(pool.clone());
    let balance_sheet = service.generate_balance_sheet(as_of_date).await.unwrap();

    // Then: 資産 = 負債 + 純資産
    let expected_total = balance_sheet.total_liabilities + balance_sheet.total_equity;
    assert_eq!(balance_sheet.total_assets, expected_total);
    assert_eq!(
        balance_sheet.total_assets,
        balance_sheet.total_liabilities_and_equity
    );

    // 具体的な金額の検証
    // 資産: 普通預金8,000,000 + 建物2,000,000 = 10,000,000
    assert_eq!(balance_sheet.total_assets, dec!(10000000));

    // 負債: 買掛金500,000 + 長期借入金4,500,000 = 5,000,000
    assert_eq!(balance_sheet.total_liabilities, dec!(5000000));

    // 純資産: 資本金5,000,000
    assert_eq!(balance_sheet.total_equity, dec!(5000000));
}

#[tokio::test]
async fn test_assets_classification() {
    // Given: テストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_balance_sheet_test_data(pool).await;

    // When: 貸借対照表を生成
    let as_of_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let service = FinancialStatementService::new(pool.clone());
    let balance_sheet = service.generate_balance_sheet(as_of_date).await.unwrap();

    // Then: 資産項目が正しく分類されている
    let assets = &balance_sheet.assets;
    assert_eq!(assets.len(), 2);

    // 流動資産（11で始まる勘定科目）
    let current_assets: Vec<_> = assets
        .iter()
        .filter(|a| a.account_code.starts_with("11"))
        .collect();
    assert_eq!(current_assets.len(), 1);
    assert_eq!(current_assets[0].account_code, "1110");
    assert_eq!(current_assets[0].account_name, "普通預金");
    assert_eq!(current_assets[0].balance, dec!(8000000));

    // 固定資産（14で始まる勘定科目）
    let fixed_assets: Vec<_> = assets
        .iter()
        .filter(|a| a.account_code.starts_with("14"))
        .collect();
    assert_eq!(fixed_assets.len(), 1);
    assert_eq!(fixed_assets[0].account_code, "1410");
    assert_eq!(fixed_assets[0].account_name, "建物");
    assert_eq!(fixed_assets[0].balance, dec!(2000000));
}

#[tokio::test]
async fn test_percentage_calculation() {
    // Given: テストデータ
    let test_db = TestDatabase::new().await;
    let pool = &test_db.pool;
    setup_balance_sheet_test_data(pool).await;

    // When: 貸借対照表を生成
    let as_of_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
    let service = FinancialStatementService::new(pool.clone());
    let balance_sheet = service.generate_balance_sheet(as_of_date).await.unwrap();

    // Then: 構成比率が計算されている
    for asset in &balance_sheet.assets {
        assert!(asset.percentage >= dec!(0));
        assert!(asset.percentage <= dec!(100));
    }

    // 普通預金の構成比率 = 8,000,000 / 10,000,000 × 100 = 80%
    let ordinary_deposit = balance_sheet
        .assets
        .iter()
        .find(|a| a.account_code == "1110")
        .unwrap();
    assert_eq!(ordinary_deposit.percentage, dec!(80.00));
}

async fn setup_balance_sheet_test_data(pool: &PgPool) {
    // テーブルをクリア
    sqlx::query(r#"TRUNCATE TABLE "日次勘定科目残高" CASCADE"#)
        .execute(pool)
        .await
        .unwrap();
    sqlx::query(r#"TRUNCATE TABLE "勘定科目マスタ" CASCADE"#)
        .execute(pool)
        .await
        .unwrap();

    // テストデータ：勘定科目マスタ（貸借対照表科目）
    // 実際に存在するカラムのみを使用
    // 貸借区分: D=借方, C=貸方
    // 集計区分: D=明細, S=サマリ, H=ヘッダー
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

    // テストデータ：日次残高（2024-01-31時点）
    let as_of_date = NaiveDate::from_ymd_opt(2024, 1, 31).unwrap();
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
    .bind(as_of_date)
    .execute(pool)
    .await
    .unwrap();
}
