mod common;

use accounting_system::domain::account::Account;
use accounting_system::repositories::account;
use common::TestDatabase;
use rust_decimal_macros::dec;

#[tokio::test]
async fn test_account_repository_insert() {
    let db = TestDatabase::new().await;

    let mut new_account = Account::new(
        "1000".to_string(),
        "現金".to_string(),
        "資産".to_string(),
        false,
    );
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

    account::insert(
        &db.pool,
        &Account::new(
            "1000".to_string(),
            "現金".to_string(),
            "資産".to_string(),
            false,
        ),
    )
    .await
    .unwrap();
    account::insert(
        &db.pool,
        &Account::new(
            "2000".to_string(),
            "買掛金".to_string(),
            "負債".to_string(),
            false,
        ),
    )
    .await
    .unwrap();
    account::insert(
        &db.pool,
        &Account::new(
            "3000".to_string(),
            "資本金".to_string(),
            "純資産".to_string(),
            false,
        ),
    )
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

    account::insert(
        &db.pool,
        &Account::new(
            "1000".to_string(),
            "現金".to_string(),
            "資産".to_string(),
            false,
        ),
    )
    .await
    .unwrap();
    account::insert(
        &db.pool,
        &Account::new(
            "1100".to_string(),
            "普通預金".to_string(),
            "資産".to_string(),
            false,
        ),
    )
    .await
    .unwrap();
    account::insert(
        &db.pool,
        &Account::new(
            "2000".to_string(),
            "買掛金".to_string(),
            "負債".to_string(),
            false,
        ),
    )
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
    account::insert(
        &db.pool,
        &Account::new(
            "11".to_string(),
            "資産の部".to_string(),
            "資産".to_string(),
            true,
        ),
    )
    .await
    .unwrap();
    account::insert(
        &db.pool,
        &Account::new(
            "11000".to_string(),
            "流動資産".to_string(),
            "資産".to_string(),
            true,
        ),
    )
    .await
    .unwrap();

    // 明細科目
    account::insert(
        &db.pool,
        &Account::new(
            "11110".to_string(),
            "現金".to_string(),
            "資産".to_string(),
            false,
        ),
    )
    .await
    .unwrap();
    account::insert(
        &db.pool,
        &Account::new(
            "11120".to_string(),
            "普通預金".to_string(),
            "資産".to_string(),
            false,
        ),
    )
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

    let mut account_data = Account::new(
        "1000".to_string(),
        "現金".to_string(),
        "資産".to_string(),
        false,
    );
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

    account::insert(
        &db.pool,
        &Account::new(
            "1000".to_string(),
            "現金".to_string(),
            "資産".to_string(),
            false,
        ),
    )
    .await
    .unwrap();

    account::delete(&db.pool, "1000").await.unwrap();

    let deleted = account::find_by_code(&db.pool, "1000").await.unwrap();
    assert!(deleted.is_none());
}
