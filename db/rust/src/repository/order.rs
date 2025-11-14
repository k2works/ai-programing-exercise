use crate::entity::{Order, OrderDetail};
use sqlx::PgPool;

pub struct OrderRepository;

impl OrderRepository {
    /// 受注データを登録
    pub async fn create(pool: &PgPool, order: &Order) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "受注データ" (
                "受注番号", "受注日", "納品予定日",
                "顧客コード", "顧客枝番", "敬称区分",
                "郵便番号", "住所１", "住所２",
                "社員コード", "部門コード",
                "明細行数", "受注金額", "消費税額", "伝票備考",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15, $16, $17, $18, $19)
            "#,
        )
        .bind(&order.order_no)
        .bind(&order.order_date)
        .bind(&order.delivery_date)
        .bind(&order.cust_code)
        .bind(&order.cust_sub_no)
        .bind(&order.title_type)
        .bind(&order.zip_code)
        .bind(&order.address1)
        .bind(&order.address2)
        .bind(&order.emp_code)
        .bind(&order.dept_code)
        .bind(&order.detail_count)
        .bind(&order.order_amount)
        .bind(order.cmp_tax)
        .bind(&order.slip_comment)
        .bind(order.create_date)
        .bind(&order.creator)
        .bind(order.update_date)
        .bind(&order.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 受注番号で受注データを取得
    pub async fn find_by_order_no(
        pool: &PgPool,
        order_no: &str,
    ) -> Result<Order, sqlx::Error> {
        sqlx::query_as::<_, Order>(r#"SELECT * FROM "受注データ" WHERE "受注番号" = $1"#)
            .bind(order_no)
            .fetch_one(pool)
            .await
    }

    /// 受注データを更新
    pub async fn update(pool: &PgPool, order: &Order) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "受注データ" SET
                "受注日" = $2,
                "納品予定日" = $3,
                "顧客コード" = $4,
                "顧客枝番" = $5,
                "敬称区分" = $6,
                "郵便番号" = $7,
                "住所１" = $8,
                "住所２" = $9,
                "社員コード" = $10,
                "部門コード" = $11,
                "明細行数" = $12,
                "受注金額" = $13,
                "消費税額" = $14,
                "伝票備考" = $15,
                "更新日時" = $16,
                "更新者名" = $17
            WHERE "受注番号" = $1
            "#,
        )
        .bind(&order.order_no)
        .bind(&order.order_date)
        .bind(&order.delivery_date)
        .bind(&order.cust_code)
        .bind(&order.cust_sub_no)
        .bind(&order.title_type)
        .bind(&order.zip_code)
        .bind(&order.address1)
        .bind(&order.address2)
        .bind(&order.emp_code)
        .bind(&order.dept_code)
        .bind(&order.detail_count)
        .bind(&order.order_amount)
        .bind(order.cmp_tax)
        .bind(&order.slip_comment)
        .bind(order.update_date)
        .bind(&order.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 受注データを削除
    pub async fn delete(pool: &PgPool, order_no: &str) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "受注データ" WHERE "受注番号" = $1"#)
            .bind(order_no)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 全ての受注データを削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "受注データ""#)
            .execute(pool)
            .await?;
        Ok(())
    }
}

pub struct OrderDetailRepository;

impl OrderDetailRepository {
    /// 受注明細を登録
    pub async fn create(pool: &PgPool, detail: &OrderDetail) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "受注データ明細" (
                "受注番号", "明細番号", "商品コード",
                "商品名", "商品名略称", "色", "サイズ",
                "数量", "単位", "単価", "売上原価",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
            "#,
        )
        .bind(&detail.order_no)
        .bind(detail.detail_no)
        .bind(&detail.prod_code)
        .bind(&detail.prod_name)
        .bind(&detail.prod_abbr_name)
        .bind(&detail.color)
        .bind(&detail.size)
        .bind(detail.quantity)
        .bind(&detail.unit)
        .bind(&detail.unit_price)
        .bind(&detail.prod_cost)
        .bind(detail.create_date)
        .bind(&detail.creator)
        .bind(detail.update_date)
        .bind(&detail.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 受注番号で受注明細を取得
    pub async fn find_by_order_no(
        pool: &PgPool,
        order_no: &str,
    ) -> Result<Vec<OrderDetail>, sqlx::Error> {
        sqlx::query_as::<_, OrderDetail>(
            r#"SELECT * FROM "受注データ明細" WHERE "受注番号" = $1 ORDER BY "明細番号""#,
        )
        .bind(order_no)
        .fetch_all(pool)
        .await
    }

    /// 受注明細を更新
    pub async fn update(pool: &PgPool, detail: &OrderDetail) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "受注データ明細" SET
                "商品コード" = $3,
                "商品名" = $4,
                "商品名略称" = $5,
                "色" = $6,
                "サイズ" = $7,
                "数量" = $8,
                "単位" = $9,
                "単価" = $10,
                "売上原価" = $11,
                "更新日時" = $12,
                "更新者名" = $13
            WHERE "受注番号" = $1 AND "明細番号" = $2
            "#,
        )
        .bind(&detail.order_no)
        .bind(detail.detail_no)
        .bind(&detail.prod_code)
        .bind(&detail.prod_name)
        .bind(&detail.prod_abbr_name)
        .bind(&detail.color)
        .bind(&detail.size)
        .bind(detail.quantity)
        .bind(&detail.unit)
        .bind(&detail.unit_price)
        .bind(&detail.prod_cost)
        .bind(detail.update_date)
        .bind(&detail.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 受注明細を削除
    pub async fn delete(
        pool: &PgPool,
        order_no: &str,
        detail_no: i32,
    ) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"DELETE FROM "受注データ明細" WHERE "受注番号" = $1 AND "明細番号" = $2"#,
        )
        .bind(order_no)
        .bind(detail_no)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 全ての受注明細を削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "受注データ明細""#)
            .execute(pool)
            .await?;
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
        OrderDetailRepository::delete_all(pool).await.ok();
        OrderRepository::delete_all(pool).await.ok();

        // Delete prerequisite data (in reverse dependency order)
        sqlx::query(r#"DELETE FROM "顧客マスタ" WHERE "顧客コード" = 'COMP001'"#).execute(pool).await.ok();
        sqlx::query(r#"DELETE FROM "社員マスタ" WHERE "社員コード" = 'EMP001'"#).execute(pool).await.ok();
        sqlx::query(r#"DELETE FROM "部門マスタ" WHERE "部門コード" = 'D001'"#).execute(pool).await.ok();
        sqlx::query(r#"DELETE FROM "取引先マスタ" WHERE "取引先コード" = 'COMP001'"#).execute(pool).await.ok();
        sqlx::query(r#"DELETE FROM "取引先グループマスタ" WHERE "取引先グループコード" = 'GRP1'"#).execute(pool).await.ok();

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
               ) VALUES ('COMP001', 'テスト会社', 'GRP1', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#
        ).execute(pool).await.expect("Failed to insert company");

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
               ) VALUES ('CAT001', 1, '/CAT001', 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#
        ).execute(pool).await.expect("Failed to insert product category");

        // 7. 商品マスタ
        sqlx::query(
            r#"INSERT INTO "商品マスタ" (
                 "商品コード", "商品正式名", "商品略称", "商品名カナ",
                 "販売単価", "売上原価", "税区分", "商品分類コード",
                 "仕入先コード", "作成日時", "更新日時"
               ) VALUES ('PROD001', 'テスト商品正式名', 'テスト商品', 'テストショウヒン', 1000, 600, 1, 'CAT001', 'COMP001', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#
        ).execute(pool).await.expect("Failed to insert product");
    }

    fn create_test_order() -> Order {
        Order {
            order_no: "ORD0000001".to_string(),
            order_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 10)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
            ),
            delivery_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 20)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
            ),
            cust_code: "COMP001".to_string(),
            cust_sub_no: Some(1),
            title_type: Some(1),
            zip_code: Some("100-0001".to_string()),
            address1: Some("東京都千代田区".to_string()),
            address2: Some("1-1-1".to_string()),
            emp_code: "EMP001".to_string(),
            dept_code: "D001".to_string(),
            detail_count: Some(2),
            order_amount: Some(20000),
            cmp_tax: 2000,
            slip_comment: Some("テスト受注".to_string()),
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

    fn create_test_order_detail() -> OrderDetail {
        OrderDetail {
            order_no: "ORD0000001".to_string(),
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
    async fn test_order_create_and_find() {
        let order = create_test_order();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create
            OrderRepository::create(&pool, &order)
                .await
                .expect("Failed to create order");

            // Find
            let found = OrderRepository::find_by_order_no(&pool, &order.order_no)
                .await
                .expect("Failed to find order");

            assert_eq!(found.order_no, order.order_no);
            assert_eq!(found.cust_code, order.cust_code);
            assert_eq!(found.order_amount, order.order_amount);
        })
        .await;
    }

    #[tokio::test]
    async fn test_order_update() {
        let mut order = create_test_order();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create
            OrderRepository::create(&pool, &order)
                .await
                .expect("Failed to create order");

            // Update
            order.order_amount = Some(30000);
            order.cmp_tax = 3000;
            OrderRepository::update(&pool, &order)
                .await
                .expect("Failed to update order");

            // Verify
            let found = OrderRepository::find_by_order_no(&pool, &order.order_no)
                .await
                .expect("Failed to find order");

            assert_eq!(found.order_amount, Some(30000));
            assert_eq!(found.cmp_tax, 3000);
        })
        .await;
    }

    #[tokio::test]
    async fn test_order_delete() {
        let order = create_test_order();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create
            OrderRepository::create(&pool, &order)
                .await
                .expect("Failed to create order");

            // Delete
            OrderRepository::delete(&pool, &order.order_no)
                .await
                .expect("Failed to delete order");

            // Verify
            let result = OrderRepository::find_by_order_no(&pool, &order.order_no).await;
            assert!(result.is_err());
        })
        .await;
    }

    #[tokio::test]
    async fn test_order_detail_create_and_find() {
        let order = create_test_order();
        let detail = create_test_order_detail();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create order first
            OrderRepository::create(&pool, &order)
                .await
                .expect("Failed to create order");

            // Create detail
            OrderDetailRepository::create(&pool, &detail)
                .await
                .expect("Failed to create order detail");

            // Find
            let found = OrderDetailRepository::find_by_order_no(&pool, &detail.order_no)
                .await
                .expect("Failed to find order details");

            assert_eq!(found.len(), 1);
            assert_eq!(found[0].order_no, detail.order_no);
            assert_eq!(found[0].detail_no, detail.detail_no);
            assert_eq!(found[0].prod_code, detail.prod_code);
        })
        .await;
    }

    #[tokio::test]
    async fn test_order_detail_update() {
        let order = create_test_order();
        let mut detail = create_test_order_detail();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create order and detail
            OrderRepository::create(&pool, &order)
                .await
                .expect("Failed to create order");
            OrderDetailRepository::create(&pool, &detail)
                .await
                .expect("Failed to create order detail");

            // Update
            detail.quantity = 20;
            detail.unit_price = Some(1500);
            OrderDetailRepository::update(&pool, &detail)
                .await
                .expect("Failed to update order detail");

            // Verify
            let found = OrderDetailRepository::find_by_order_no(&pool, &detail.order_no)
                .await
                .expect("Failed to find order details");

            assert_eq!(found[0].quantity, 20);
            assert_eq!(found[0].unit_price, Some(1500));
        })
        .await;
    }

    #[tokio::test]
    async fn test_order_detail_delete() {
        let order = create_test_order();
        let detail = create_test_order_detail();
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // Create order and detail
            OrderRepository::create(&pool, &order)
                .await
                .expect("Failed to create order");
            OrderDetailRepository::create(&pool, &detail)
                .await
                .expect("Failed to create order detail");

            // Delete
            OrderDetailRepository::delete(&pool, &detail.order_no, detail.detail_no)
                .await
                .expect("Failed to delete order detail");

            // Verify
            let found = OrderDetailRepository::find_by_order_no(&pool, &detail.order_no)
                .await
                .expect("Failed to find order details");

            assert_eq!(found.len(), 0);
        })
        .await;
    }
}
