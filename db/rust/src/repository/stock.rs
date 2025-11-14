use crate::entity::Stock;
use sqlx::PgPool;

pub struct StockRepository;

impl StockRepository {
    /// 在庫を登録
    pub async fn create(pool: &PgPool, stock: &Stock) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "在庫データ" (
                "倉庫コード", "商品コード", "ロット番号",
                "在庫区分", "良品区分",
                "実在庫数", "有効在庫数", "最終出荷日",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12)
            "#,
        )
        .bind(&stock.wh_code)
        .bind(&stock.prod_code)
        .bind(&stock.lot_no)
        .bind(&stock.stock_type)
        .bind(&stock.quality_type)
        .bind(stock.actual)
        .bind(stock.valid)
        .bind(stock.last_delivery_date)
        .bind(stock.create_date)
        .bind(&stock.creator)
        .bind(stock.update_date)
        .bind(&stock.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 複合キーで在庫を取得
    pub async fn find_by_key(
        pool: &PgPool,
        wh_code: &str,
        prod_code: &str,
        lot_no: &str,
        stock_type: &str,
        quality_type: &str,
    ) -> Result<Stock, sqlx::Error> {
        sqlx::query_as::<_, Stock>(
            r#"SELECT * FROM "在庫データ" WHERE "倉庫コード" = $1 AND "商品コード" = $2 AND "ロット番号" = $3 AND "在庫区分" = $4 AND "良品区分" = $5"#,
        )
        .bind(wh_code)
        .bind(prod_code)
        .bind(lot_no)
        .bind(stock_type)
        .bind(quality_type)
        .fetch_one(pool)
        .await
    }

    /// 倉庫コードで在庫を取得
    pub async fn find_by_wh_code(
        pool: &PgPool,
        wh_code: &str,
    ) -> Result<Vec<Stock>, sqlx::Error> {
        sqlx::query_as::<_, Stock>(
            r#"SELECT * FROM "在庫データ" WHERE "倉庫コード" = $1 ORDER BY "商品コード", "ロット番号""#,
        )
        .bind(wh_code)
        .fetch_all(pool)
        .await
    }

    /// 商品コードで在庫を取得
    pub async fn find_by_prod_code(
        pool: &PgPool,
        prod_code: &str,
    ) -> Result<Vec<Stock>, sqlx::Error> {
        sqlx::query_as::<_, Stock>(
            r#"SELECT * FROM "在庫データ" WHERE "商品コード" = $1 ORDER BY "倉庫コード", "ロット番号""#,
        )
        .bind(prod_code)
        .fetch_all(pool)
        .await
    }

    /// 在庫を更新
    pub async fn update(pool: &PgPool, stock: &Stock) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "在庫データ" SET
                "実在庫数" = $6,
                "有効在庫数" = $7,
                "最終出荷日" = $8,
                "更新日時" = $9,
                "更新者名" = $10
            WHERE "倉庫コード" = $1 AND "商品コード" = $2 AND "ロット番号" = $3 AND "在庫区分" = $4 AND "良品区分" = $5
            "#,
        )
        .bind(&stock.wh_code)
        .bind(&stock.prod_code)
        .bind(&stock.lot_no)
        .bind(&stock.stock_type)
        .bind(&stock.quality_type)
        .bind(stock.actual)
        .bind(stock.valid)
        .bind(stock.last_delivery_date)
        .bind(stock.update_date)
        .bind(&stock.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 在庫を削除
    pub async fn delete(
        pool: &PgPool,
        wh_code: &str,
        prod_code: &str,
        lot_no: &str,
        stock_type: &str,
        quality_type: &str,
    ) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"DELETE FROM "在庫データ" WHERE "倉庫コード" = $1 AND "商品コード" = $2 AND "ロット番号" = $3 AND "在庫区分" = $4 AND "良品区分" = $5"#,
        )
        .bind(wh_code)
        .bind(prod_code)
        .bind(lot_no)
        .bind(stock_type)
        .bind(quality_type)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 全ての在庫を削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "在庫データ""#)
            .execute(pool)
            .await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::repository::{ProductCategoryRepository, ProductRepository, WarehouseRepository};
    use crate::test_support::with_test_pool;
    use chrono::NaiveDate;

    async fn setup(pool: &PgPool) {
        // テーブルをクリーンアップ（依存関係の逆順）
        StockRepository::delete_all(pool).await.ok();
        WarehouseRepository::delete_all(pool).await.ok();
        ProductRepository::delete_all(pool).await.ok();
        ProductCategoryRepository::delete_all(pool).await.ok();

        // 取引先グループ、取引先、商品分類、商品、倉庫を登録
        sqlx::query(
            r#"INSERT INTO "取引先グループマスタ" (
                "取引先グループコード", "取引先グループ名",
                "作成日時", "更新日時"
            ) VALUES ('GRP1', 'テストグループ', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .expect("Failed to insert company group");

        sqlx::query(
            r#"INSERT INTO "取引先マスタ" (
                "取引先コード", "取引先名", "取引先名カナ",
                "取引先グループコード",
                "作成日時", "更新日時"
            ) VALUES ('COMP001', 'テスト取引先', 'テストトリヒキサキ', 'GRP1', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .expect("Failed to insert company");

        sqlx::query(
            r#"INSERT INTO "商品分類マスタ" (
                "商品分類コード", "商品分類名",
                "作成日時", "更新日時"
            ) VALUES ('CAT001', 'テスト分類', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .expect("Failed to insert product category");

        sqlx::query(
            r#"INSERT INTO "商品マスタ" (
                "商品コード", "商品正式名", "商品略称", "商品名カナ",
                "販売単価", "売上原価", "税区分", "商品分類コード",
                "仕入先コード", "作成日時", "更新日時"
            ) VALUES ('PROD001', 'テスト商品正式名', 'テスト商品', 'テストショウヒン', 1000, 600, 1, 'CAT001', 'COMP001', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .expect("Failed to insert product");

        sqlx::query(
            r#"INSERT INTO "倉庫マスタ" (
                "倉庫コード", "倉庫名", "倉庫名略称",
                "作成日時", "更新日時"
            ) VALUES ('WH1', '東京倉庫', '東京', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .expect("Failed to insert warehouse");
    }

    fn create_test_stock() -> Stock {
        Stock {
            wh_code: "WH1".to_string(),
            prod_code: "PROD001".to_string(),
            lot_no: "LOT20210101".to_string(),
            stock_type: "1".to_string(),
            quality_type: "G".to_string(),
            actual: 100,
            valid: 100,
            last_delivery_date: None,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                .unwrap()
                .and_hms_opt(0, 0, 0)
                .unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[tokio::test]
    async fn test_stock_create_and_find() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let stock = create_test_stock();

            // 在庫を登録
            StockRepository::create(&pool, &stock)
                .await
                .expect("Failed to create stock");

            // 登録した在庫を取得
            let found = StockRepository::find_by_key(
                &pool,
                &stock.wh_code,
                &stock.prod_code,
                &stock.lot_no,
                &stock.stock_type,
                &stock.quality_type,
            )
            .await
            .expect("Failed to find stock");

            assert_eq!(found.wh_code, stock.wh_code);
            assert_eq!(found.prod_code, stock.prod_code);
            assert_eq!(found.actual, stock.actual);
            assert_eq!(found.valid, stock.valid);
        })
        .await;
    }

    #[tokio::test]
    async fn test_stock_update() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let mut stock = create_test_stock();

            // 在庫を登録
            StockRepository::create(&pool, &stock)
                .await
                .expect("Failed to create stock");

            // 在庫数を更新
            stock.actual = 150;
            stock.valid = 150;
            StockRepository::update(&pool, &stock)
                .await
                .expect("Failed to update stock");

            // 更新された在庫を取得
            let updated = StockRepository::find_by_key(
                &pool,
                &stock.wh_code,
                &stock.prod_code,
                &stock.lot_no,
                &stock.stock_type,
                &stock.quality_type,
            )
            .await
            .expect("Failed to find stock");

            assert_eq!(updated.actual, 150);
            assert_eq!(updated.valid, 150);
        })
        .await;
    }

    #[tokio::test]
    async fn test_stock_delete() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let stock = create_test_stock();

            // 在庫を登録
            StockRepository::create(&pool, &stock)
                .await
                .expect("Failed to create stock");

            // 在庫を削除
            StockRepository::delete(
                &pool,
                &stock.wh_code,
                &stock.prod_code,
                &stock.lot_no,
                &stock.stock_type,
                &stock.quality_type,
            )
            .await
            .expect("Failed to delete stock");

            // 削除された在庫を取得しようとするとエラー
            let result = StockRepository::find_by_key(
                &pool,
                &stock.wh_code,
                &stock.prod_code,
                &stock.lot_no,
                &stock.stock_type,
                &stock.quality_type,
            )
            .await;
            assert!(result.is_err());
        })
        .await;
    }

    #[tokio::test]
    async fn test_stock_find_by_wh_code() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let stock1 = create_test_stock();
            let mut stock2 = create_test_stock();
            stock2.lot_no = "LOT20210102".to_string();
            stock2.actual = 50;

            // 2つの在庫を登録
            StockRepository::create(&pool, &stock1)
                .await
                .expect("Failed to create stock1");
            StockRepository::create(&pool, &stock2)
                .await
                .expect("Failed to create stock2");

            // 倉庫コードで在庫を検索
            let stocks = StockRepository::find_by_wh_code(&pool, &stock1.wh_code)
                .await
                .expect("Failed to find stocks by warehouse");

            assert_eq!(stocks.len(), 2);
        })
        .await;
    }

    #[tokio::test]
    async fn test_stock_find_by_prod_code() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let stock1 = create_test_stock();
            let mut stock2 = create_test_stock();
            stock2.lot_no = "LOT20210102".to_string();

            // 2つの在庫を登録
            StockRepository::create(&pool, &stock1)
                .await
                .expect("Failed to create stock1");
            StockRepository::create(&pool, &stock2)
                .await
                .expect("Failed to create stock2");

            // 商品コードで在庫を検索
            let stocks = StockRepository::find_by_prod_code(&pool, &stock1.prod_code)
                .await
                .expect("Failed to find stocks by product");

            assert_eq!(stocks.len(), 2);
        })
        .await;
    }
}
