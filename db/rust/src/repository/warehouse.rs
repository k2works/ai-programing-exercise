use crate::entity::Warehouse;
use sqlx::PgPool;

pub struct WarehouseRepository;

impl WarehouseRepository {
    /// 倉庫を登録
    pub async fn create(pool: &PgPool, warehouse: &Warehouse) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "倉庫マスタ" (
                "倉庫コード", "倉庫名", "倉庫名略称",
                "郵便番号", "都道府県", "住所１", "住所２",
                "電話番号", "ＦＡＸ番号",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
            "#,
        )
        .bind(&warehouse.wh_code)
        .bind(&warehouse.wh_name)
        .bind(&warehouse.wh_abbr_name)
        .bind(&warehouse.zip_code)
        .bind(&warehouse.state)
        .bind(&warehouse.address1)
        .bind(&warehouse.address2)
        .bind(&warehouse.tel)
        .bind(&warehouse.fax)
        .bind(warehouse.create_date)
        .bind(&warehouse.creator)
        .bind(warehouse.update_date)
        .bind(&warehouse.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 倉庫コードで倉庫を取得
    pub async fn find_by_wh_code(pool: &PgPool, wh_code: &str) -> Result<Warehouse, sqlx::Error> {
        sqlx::query_as::<_, Warehouse>(r#"SELECT * FROM "倉庫マスタ" WHERE "倉庫コード" = $1"#)
            .bind(wh_code)
            .fetch_one(pool)
            .await
    }

    /// 全ての倉庫を取得
    pub async fn find_all(pool: &PgPool) -> Result<Vec<Warehouse>, sqlx::Error> {
        sqlx::query_as::<_, Warehouse>(r#"SELECT * FROM "倉庫マスタ" ORDER BY "倉庫コード""#)
            .fetch_all(pool)
            .await
    }

    /// 倉庫を更新
    pub async fn update(pool: &PgPool, warehouse: &Warehouse) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "倉庫マスタ" SET
                "倉庫名" = $2,
                "倉庫名略称" = $3,
                "郵便番号" = $4,
                "都道府県" = $5,
                "住所１" = $6,
                "住所２" = $7,
                "電話番号" = $8,
                "ＦＡＸ番号" = $9,
                "更新日時" = $10,
                "更新者名" = $11
            WHERE "倉庫コード" = $1
            "#,
        )
        .bind(&warehouse.wh_code)
        .bind(&warehouse.wh_name)
        .bind(&warehouse.wh_abbr_name)
        .bind(&warehouse.zip_code)
        .bind(&warehouse.state)
        .bind(&warehouse.address1)
        .bind(&warehouse.address2)
        .bind(&warehouse.tel)
        .bind(&warehouse.fax)
        .bind(warehouse.update_date)
        .bind(&warehouse.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 倉庫を削除
    pub async fn delete(pool: &PgPool, wh_code: &str) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "倉庫マスタ" WHERE "倉庫コード" = $1"#)
            .bind(wh_code)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 全ての倉庫を削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "倉庫マスタ""#).execute(pool).await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::with_test_pool;
    use chrono::NaiveDate;

    async fn setup(pool: &PgPool) {
        // テーブルをクリーンアップ
        WarehouseRepository::delete_all(pool).await.ok();
    }

    fn create_test_warehouse() -> Warehouse {
        Warehouse {
            wh_code: "WH1".to_string(),
            wh_name: Some("東京倉庫".to_string()),
            wh_abbr_name: Some("東京".to_string()),
            zip_code: Some("100-0001".to_string()),
            state: Some("東京都".to_string()),
            address1: Some("千代田区".to_string()),
            address2: Some("1-1-1".to_string()),
            tel: Some("03-1234-5678".to_string()),
            fax: Some("03-1234-5679".to_string()),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[tokio::test]
    async fn test_warehouse_create_and_find() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let warehouse = create_test_warehouse();

            // 倉庫を登録
            WarehouseRepository::create(&pool, &warehouse)
                .await
                .expect("Failed to create warehouse");

            // 登録した倉庫を取得
            let found = WarehouseRepository::find_by_wh_code(&pool, &warehouse.wh_code)
                .await
                .expect("Failed to find warehouse");

            assert_eq!(found.wh_code, warehouse.wh_code);
            assert_eq!(found.wh_name, warehouse.wh_name);
            assert_eq!(found.wh_abbr_name, warehouse.wh_abbr_name);
        })
        .await;
    }

    #[tokio::test]
    async fn test_warehouse_update() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let mut warehouse = create_test_warehouse();

            // 倉庫を登録
            WarehouseRepository::create(&pool, &warehouse)
                .await
                .expect("Failed to create warehouse");

            // 倉庫名を更新
            warehouse.wh_name = Some("大阪倉庫".to_string());
            WarehouseRepository::update(&pool, &warehouse)
                .await
                .expect("Failed to update warehouse");

            // 更新された倉庫を取得
            let updated = WarehouseRepository::find_by_wh_code(&pool, &warehouse.wh_code)
                .await
                .expect("Failed to find warehouse");

            assert_eq!(updated.wh_name, Some("大阪倉庫".to_string()));
        })
        .await;
    }

    #[tokio::test]
    async fn test_warehouse_delete() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let warehouse = create_test_warehouse();

            // 倉庫を登録
            WarehouseRepository::create(&pool, &warehouse)
                .await
                .expect("Failed to create warehouse");

            // 倉庫を削除
            WarehouseRepository::delete(&pool, &warehouse.wh_code)
                .await
                .expect("Failed to delete warehouse");

            // 削除された倉庫を取得しようとするとエラー
            let result = WarehouseRepository::find_by_wh_code(&pool, &warehouse.wh_code).await;
            assert!(result.is_err());
        })
        .await;
    }

    #[tokio::test]
    async fn test_warehouse_find_all() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let warehouse1 = create_test_warehouse();
            let mut warehouse2 = create_test_warehouse();
            warehouse2.wh_code = "WH2".to_string();
            warehouse2.wh_name = Some("大阪倉庫".to_string());

            // 2つの倉庫を登録
            WarehouseRepository::create(&pool, &warehouse1)
                .await
                .expect("Failed to create warehouse1");
            WarehouseRepository::create(&pool, &warehouse2)
                .await
                .expect("Failed to create warehouse2");

            // 全ての倉庫を取得
            let warehouses =
                WarehouseRepository::find_all(&pool).await.expect("Failed to find all warehouses");

            assert_eq!(warehouses.len(), 2);
        })
        .await;
    }
}
