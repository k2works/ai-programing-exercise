use crate::entity::{Sales, SalesDetail};
use sqlx::PgPool;

pub struct SalesRepository;

impl SalesRepository {
    /// 売上データを登録
    pub async fn create(pool: &PgPool, sales: &Sales) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "売上データ" (
                "売上番号", "売上日", "顧客コード", "顧客枝番",
                "社員コード", "部門コード", "開始日", "倉庫コード",
                "受注番号", "明細行数", "売上金額", "消費税額", "伝票備考",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17)
            "#,
        )
        .bind(&sales.sales_no)
        .bind(sales.sales_date)
        .bind(&sales.cust_code)
        .bind(sales.cust_sub_no)
        .bind(&sales.emp_code)
        .bind(&sales.dept_code)
        .bind(sales.start_date)
        .bind(&sales.wh_code)
        .bind(&sales.order_no)
        .bind(sales.detail_count)
        .bind(sales.sales_amount)
        .bind(sales.cmp_tax)
        .bind(&sales.slip_comment)
        .bind(sales.create_date)
        .bind(&sales.creator)
        .bind(sales.update_date)
        .bind(&sales.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 売上番号で売上データを取得
    pub async fn find_by_sales_no(pool: &PgPool, sales_no: &str) -> Result<Sales, sqlx::Error> {
        sqlx::query_as::<_, Sales>(r#"SELECT * FROM "売上データ" WHERE "売上番号" = $1"#)
            .bind(sales_no)
            .fetch_one(pool)
            .await
    }

    /// 受注番号で売上データを取得
    pub async fn find_by_order_no(
        pool: &PgPool,
        order_no: &str,
    ) -> Result<Vec<Sales>, sqlx::Error> {
        sqlx::query_as::<_, Sales>(r#"SELECT * FROM "売上データ" WHERE "受注番号" = $1"#)
            .bind(order_no)
            .fetch_all(pool)
            .await
    }

    /// 売上データを更新
    pub async fn update(pool: &PgPool, sales: &Sales) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "売上データ" SET
                "売上日" = $2,
                "顧客コード" = $3,
                "顧客枝番" = $4,
                "社員コード" = $5,
                "部門コード" = $6,
                "開始日" = $7,
                "倉庫コード" = $8,
                "受注番号" = $9,
                "明細行数" = $10,
                "売上金額" = $11,
                "消費税額" = $12,
                "伝票備考" = $13,
                "更新日時" = $14,
                "更新者名" = $15
            WHERE "売上番号" = $1
            "#,
        )
        .bind(&sales.sales_no)
        .bind(sales.sales_date)
        .bind(&sales.cust_code)
        .bind(sales.cust_sub_no)
        .bind(&sales.emp_code)
        .bind(&sales.dept_code)
        .bind(sales.start_date)
        .bind(&sales.wh_code)
        .bind(&sales.order_no)
        .bind(sales.detail_count)
        .bind(sales.sales_amount)
        .bind(sales.cmp_tax)
        .bind(&sales.slip_comment)
        .bind(sales.update_date)
        .bind(&sales.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 売上データを削除
    pub async fn delete(pool: &PgPool, sales_no: &str) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "売上データ" WHERE "売上番号" = $1"#)
            .bind(sales_no)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 全ての売上データを削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "売上データ""#).execute(pool).await?;
        Ok(())
    }
}

pub struct SalesDetailRepository;

impl SalesDetailRepository {
    /// 売上明細を登録
    pub async fn create(pool: &PgPool, detail: &SalesDetail) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "売上データ明細" (
                "売上番号", "明細番号", "商品コード",
                "商品名", "商品名略称", "色", "サイズ",
                "数量", "単位", "単価", "売上原価",
                "受注番号", "受注明細番号",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17)
            "#,
        )
        .bind(&detail.sales_no)
        .bind(detail.detail_no)
        .bind(&detail.prod_code)
        .bind(&detail.prod_name)
        .bind(&detail.prod_abbr_name)
        .bind(&detail.color)
        .bind(&detail.size)
        .bind(detail.quantity)
        .bind(&detail.unit)
        .bind(detail.unit_price)
        .bind(detail.prod_cost)
        .bind(&detail.order_no)
        .bind(detail.order_detail_no)
        .bind(detail.create_date)
        .bind(&detail.creator)
        .bind(detail.update_date)
        .bind(&detail.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 売上番号で売上明細を取得
    pub async fn find_by_sales_no(
        pool: &PgPool,
        sales_no: &str,
    ) -> Result<Vec<SalesDetail>, sqlx::Error> {
        sqlx::query_as::<_, SalesDetail>(
            r#"SELECT * FROM "売上データ明細" WHERE "売上番号" = $1 ORDER BY "明細番号""#,
        )
        .bind(sales_no)
        .fetch_all(pool)
        .await
    }

    /// 受注番号で売上明細を取得
    pub async fn find_by_order_no(
        pool: &PgPool,
        order_no: &str,
    ) -> Result<Vec<SalesDetail>, sqlx::Error> {
        sqlx::query_as::<_, SalesDetail>(
            r#"SELECT * FROM "売上データ明細" WHERE "受注番号" = $1 ORDER BY "受注明細番号""#,
        )
        .bind(order_no)
        .fetch_all(pool)
        .await
    }

    /// 売上明細を更新
    pub async fn update(pool: &PgPool, detail: &SalesDetail) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "売上データ明細" SET
                "商品コード" = $3,
                "商品名" = $4,
                "商品名略称" = $5,
                "色" = $6,
                "サイズ" = $7,
                "数量" = $8,
                "単位" = $9,
                "単価" = $10,
                "売上原価" = $11,
                "受注番号" = $12,
                "受注明細番号" = $13,
                "更新日時" = $14,
                "更新者名" = $15
            WHERE "売上番号" = $1 AND "明細番号" = $2
            "#,
        )
        .bind(&detail.sales_no)
        .bind(detail.detail_no)
        .bind(&detail.prod_code)
        .bind(&detail.prod_name)
        .bind(&detail.prod_abbr_name)
        .bind(&detail.color)
        .bind(&detail.size)
        .bind(detail.quantity)
        .bind(&detail.unit)
        .bind(detail.unit_price)
        .bind(detail.prod_cost)
        .bind(&detail.order_no)
        .bind(detail.order_detail_no)
        .bind(detail.update_date)
        .bind(&detail.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 売上明細を削除
    pub async fn delete(pool: &PgPool, sales_no: &str, detail_no: i32) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "売上データ明細" WHERE "売上番号" = $1 AND "明細番号" = $2"#)
            .bind(sales_no)
            .bind(detail_no)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 全ての売上明細を削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "売上データ明細""#).execute(pool).await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::with_test_pool;
    use chrono::NaiveDate;

    async fn setup(pool: &PgPool) {
        // Delete dependent records first
        SalesDetailRepository::delete_all(pool).await.ok();
        SalesRepository::delete_all(pool).await.ok();

        // Delete prerequisite data (in reverse dependency order)
        sqlx::query(r#"DELETE FROM "顧客マスタ" WHERE "顧客コード" = 'COMP001'"#)
            .execute(pool)
            .await
            .ok();
        sqlx::query(r#"DELETE FROM "社員マスタ" WHERE "社員コード" = 'EMP001'"#)
            .execute(pool)
            .await
            .ok();
        sqlx::query(r#"DELETE FROM "部門マスタ" WHERE "部門コード" = 'D001'"#)
            .execute(pool)
            .await
            .ok();
        sqlx::query(r#"DELETE FROM "取引先マスタ" WHERE "取引先コード" = 'COMP001'"#)
            .execute(pool)
            .await
            .ok();
        sqlx::query(r#"DELETE FROM "取引先グループマスタ" WHERE "取引先グループコード" = 'GRP1'"#)
            .execute(pool)
            .await
            .ok();

        // Create prerequisite data for foreign key constraints
        // 1. 取引先グループマスタ
        sqlx::query(
            r#"INSERT INTO "取引先グループマスタ" ("取引先グループコード", "取引先グループ名", "作成日時", "更新日時")
               VALUES ('GRP1', 'テストグループ', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#
        ).execute(pool).await.expect("Failed to insert company group");

        // 2. 取引先マスタ
        sqlx::query(
            r#"INSERT INTO "取引先マスタ" (
                 "取引先コード", "取引先名", "取引先グループコード", "作成日時", "更新日時"
               ) VALUES ('COMP001', 'テスト会社', 'GRP1', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .expect("Failed to insert company");

        // 3. 部門マスタ
        sqlx::query(
            r#"INSERT INTO "部門マスタ" (
                 "部門コード", "開始日", "部門名", "組織階層", "部門パス", "最下層区分", "伝票入力可否",
                 "作成日時", "更新日時"
               ) VALUES ('D001', '2021-01-01', 'テスト部門', 1, '/D001', 1, 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#
        ).execute(pool).await.expect("Failed to insert department");

        // 4. 社員マスタ
        sqlx::query(
            r#"INSERT INTO "社員マスタ" (
                 "社員コード", "社員名", "部門コード", "開始日", "職種コード", "承認権限コード",
                 "作成日時", "更新日時"
               ) VALUES ('EMP001', 'テスト社員', 'D001', '2021-01-01', '01', '01', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#
        ).execute(pool).await.expect("Failed to insert employee");

        // 5. 顧客マスタ
        sqlx::query(
            r#"INSERT INTO "顧客マスタ" (
                 "顧客コード", "顧客枝番", "請求先コード", "回収先コード",
                 "顧客名", "自社担当者コード", "顧客締日１",
                 "作成日時", "更新日時"
               ) VALUES ('COMP001', 1, 'COMP001', 'COMP001', 'テスト顧客', 'EMP001', 31, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#
        ).execute(pool).await.expect("Failed to insert customer");

        // 6. 商品分類マスタ
        sqlx::query(
            r#"INSERT INTO "商品分類マスタ" (
                 "商品分類コード", "商品分類階層", "商品分類パス", "最下層区分",
                 "作成日時", "更新日時"
               ) VALUES ('CAT001', 1, '/CAT001', 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .expect("Failed to insert product category");

        // 7. 商品マスタ
        sqlx::query(
            r#"INSERT INTO "商品マスタ" (
                 "商品コード", "商品正式名", "商品略称", "商品名カナ",
                 "販売単価", "売上原価", "税区分", "商品分類コード",
                 "仕入先コード", "作成日時", "更新日時"
               ) VALUES ('PROD001', 'テスト商品正式名', 'テスト商品', 'テストショウヒン', 1000, 600, 1, 'CAT001', 'COMP001', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#
        ).execute(pool).await.expect("Failed to insert product");
    }

    fn create_test_sales() -> Sales {
        Sales {
            sales_no: "SAL0000001".to_string(),
            sales_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 15).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            ),
            cust_code: "COMP001".to_string(),
            cust_sub_no: Some(1),
            emp_code: "EMP001".to_string(),
            dept_code: "D001".to_string(),
            start_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            wh_code: "WH1".to_string(),
            order_no: Some("ORD0000001".to_string()),
            detail_count: Some(2),
            sales_amount: Some(20000),
            cmp_tax: 2000,
            slip_comment: Some("テスト売上".to_string()),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    fn create_test_sales_detail() -> SalesDetail {
        SalesDetail {
            sales_no: "SAL0000001".to_string(),
            detail_no: 1,
            prod_code: "PROD001".to_string(),
            prod_name: Some("テスト商品".to_string()),
            prod_abbr_name: Some("テスト".to_string()),
            color: Some("赤".to_string()),
            size: Some("M".to_string()),
            quantity: 10,
            unit: Some("個".to_string()),
            unit_price: Some(1000),
            prod_cost: Some(600),
            order_no: Some("ORD0000001".to_string()),
            order_detail_no: Some(1),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[tokio::test]
    async fn test_sales_create_and_find() {
        let sales = create_test_sales();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create
            SalesRepository::create(&pool, &sales).await.expect("Failed to create sales");

            // Find
            let found = SalesRepository::find_by_sales_no(&pool, &sales.sales_no)
                .await
                .expect("Failed to find sales");

            assert_eq!(found.sales_no, sales.sales_no);
            assert_eq!(found.cust_code, sales.cust_code);
            assert_eq!(found.sales_amount, sales.sales_amount);
        })
        .await;
    }

    #[tokio::test]
    async fn test_sales_find_by_order_no() {
        let sales = create_test_sales();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create
            SalesRepository::create(&pool, &sales).await.expect("Failed to create sales");

            // Find by order_no
            let found = SalesRepository::find_by_order_no(&pool, "ORD0000001")
                .await
                .expect("Failed to find sales by order_no");

            assert_eq!(found.len(), 1);
            assert_eq!(found[0].sales_no, sales.sales_no);
            assert_eq!(found[0].order_no, Some("ORD0000001".to_string()));
        })
        .await;
    }

    #[tokio::test]
    async fn test_sales_update() {
        let mut sales = create_test_sales();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create
            SalesRepository::create(&pool, &sales).await.expect("Failed to create sales");

            // Update
            sales.sales_amount = Some(30000);
            sales.cmp_tax = 3000;
            SalesRepository::update(&pool, &sales).await.expect("Failed to update sales");

            // Verify
            let found = SalesRepository::find_by_sales_no(&pool, &sales.sales_no)
                .await
                .expect("Failed to find sales");

            assert_eq!(found.sales_amount, Some(30000));
            assert_eq!(found.cmp_tax, 3000);
        })
        .await;
    }

    #[tokio::test]
    async fn test_sales_delete() {
        let sales = create_test_sales();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create
            SalesRepository::create(&pool, &sales).await.expect("Failed to create sales");

            // Delete
            SalesRepository::delete(&pool, &sales.sales_no).await.expect("Failed to delete sales");

            // Verify
            let result = SalesRepository::find_by_sales_no(&pool, &sales.sales_no).await;
            assert!(result.is_err());
        })
        .await;
    }

    #[tokio::test]
    async fn test_sales_detail_create_and_find() {
        let sales = create_test_sales();
        let detail = create_test_sales_detail();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create sales first
            SalesRepository::create(&pool, &sales).await.expect("Failed to create sales");

            // Create detail
            SalesDetailRepository::create(&pool, &detail)
                .await
                .expect("Failed to create sales detail");

            // Find
            let found = SalesDetailRepository::find_by_sales_no(&pool, &detail.sales_no)
                .await
                .expect("Failed to find sales details");

            assert_eq!(found.len(), 1);
            assert_eq!(found[0].sales_no, detail.sales_no);
            assert_eq!(found[0].detail_no, detail.detail_no);
            assert_eq!(found[0].prod_code, detail.prod_code);
        })
        .await;
    }

    #[tokio::test]
    async fn test_sales_detail_find_by_order_no() {
        let sales = create_test_sales();
        let detail = create_test_sales_detail();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create sales and detail
            SalesRepository::create(&pool, &sales).await.expect("Failed to create sales");
            SalesDetailRepository::create(&pool, &detail)
                .await
                .expect("Failed to create sales detail");

            // Find by order_no
            let found = SalesDetailRepository::find_by_order_no(&pool, "ORD0000001")
                .await
                .expect("Failed to find sales details by order_no");

            assert_eq!(found.len(), 1);
            assert_eq!(found[0].order_no, Some("ORD0000001".to_string()));
            assert_eq!(found[0].order_detail_no, Some(1));
        })
        .await;
    }

    #[tokio::test]
    async fn test_sales_detail_update() {
        let sales = create_test_sales();
        let mut detail = create_test_sales_detail();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create sales and detail
            SalesRepository::create(&pool, &sales).await.expect("Failed to create sales");
            SalesDetailRepository::create(&pool, &detail)
                .await
                .expect("Failed to create sales detail");

            // Update
            detail.quantity = 20;
            detail.unit_price = Some(1500);
            SalesDetailRepository::update(&pool, &detail)
                .await
                .expect("Failed to update sales detail");

            // Verify
            let found = SalesDetailRepository::find_by_sales_no(&pool, &detail.sales_no)
                .await
                .expect("Failed to find sales details");

            assert_eq!(found[0].quantity, 20);
            assert_eq!(found[0].unit_price, Some(1500));
        })
        .await;
    }

    #[tokio::test]
    async fn test_sales_detail_delete() {
        let sales = create_test_sales();
        let detail = create_test_sales_detail();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create sales and detail
            SalesRepository::create(&pool, &sales).await.expect("Failed to create sales");
            SalesDetailRepository::create(&pool, &detail)
                .await
                .expect("Failed to create sales detail");

            // Delete
            SalesDetailRepository::delete(&pool, &detail.sales_no, detail.detail_no)
                .await
                .expect("Failed to delete sales detail");

            // Verify
            let found = SalesDetailRepository::find_by_sales_no(&pool, &detail.sales_no)
                .await
                .expect("Failed to find sales details");

            assert_eq!(found.len(), 0);
        })
        .await;
    }
}
