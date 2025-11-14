use crate::entity::{PurchaseOrder, PurchaseOrderDetail};
use sqlx::PgPool;

pub struct PurchaseOrderRepository;

impl PurchaseOrderRepository {
    /// 発注を登録
    pub async fn create(pool: &PgPool, po: &PurchaseOrder) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "発注データ" (
                "発注番号", "発注日", "仕入先コード", "仕入先枝番",
                "発注担当者コード", "指定納期", "倉庫コード",
                "発注金額合計", "消費税合計", "備考",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)
            "#,
        )
        .bind(&po.po_no)
        .bind(po.po_date)
        .bind(&po.sup_code)
        .bind(po.sup_sub_no)
        .bind(&po.emp_code)
        .bind(po.due_date)
        .bind(&po.wh_code)
        .bind(po.po_amount)
        .bind(po.cmp_tax)
        .bind(&po.slip_comment)
        .bind(po.create_date)
        .bind(&po.creator)
        .bind(po.update_date)
        .bind(&po.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 発注番号で発注を取得
    pub async fn find_by_po_no(pool: &PgPool, po_no: &str) -> Result<PurchaseOrder, sqlx::Error> {
        sqlx::query_as::<_, PurchaseOrder>(r#"SELECT * FROM "発注データ" WHERE "発注番号" = $1"#)
            .bind(po_no)
            .fetch_one(pool)
            .await
    }

    /// 発注を更新
    pub async fn update(pool: &PgPool, po: &PurchaseOrder) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "発注データ" SET
                "発注日" = $2,
                "仕入先コード" = $3,
                "仕入先枝番" = $4,
                "発注担当者コード" = $5,
                "指定納期" = $6,
                "倉庫コード" = $7,
                "発注金額合計" = $8,
                "消費税合計" = $9,
                "備考" = $10,
                "更新日時" = $11,
                "更新者名" = $12
            WHERE "発注番号" = $1
            "#,
        )
        .bind(&po.po_no)
        .bind(po.po_date)
        .bind(&po.sup_code)
        .bind(po.sup_sub_no)
        .bind(&po.emp_code)
        .bind(po.due_date)
        .bind(&po.wh_code)
        .bind(po.po_amount)
        .bind(po.cmp_tax)
        .bind(&po.slip_comment)
        .bind(po.update_date)
        .bind(&po.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 発注を削除
    pub async fn delete(pool: &PgPool, po_no: &str) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "発注データ" WHERE "発注番号" = $1"#)
            .bind(po_no)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 全ての発注を削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "発注データ""#).execute(pool).await?;
        Ok(())
    }
}

pub struct PurchaseOrderDetailRepository;

impl PurchaseOrderDetailRepository {
    /// 発注明細を登録
    pub async fn create(pool: &PgPool, detail: &PurchaseOrderDetail) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "発注データ明細" (
                "発注番号", "発注行番号", "商品コード", "商品名",
                "発注数量", "単価", "入荷予定数量", "入荷済数量", "完了フラグ",
                "作成日時", "作成者名", "更新日時", "更新者名"
            ) VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13)
            "#,
        )
        .bind(&detail.po_no)
        .bind(detail.po_row_no)
        .bind(&detail.prod_code)
        .bind(&detail.prod_name)
        .bind(detail.quantity)
        .bind(detail.unit_price)
        .bind(detail.expected_qty)
        .bind(detail.received_qty)
        .bind(detail.complete_flg)
        .bind(detail.create_date)
        .bind(&detail.creator)
        .bind(detail.update_date)
        .bind(&detail.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 発注番号で発注明細を取得
    pub async fn find_by_po_no(
        pool: &PgPool,
        po_no: &str,
    ) -> Result<Vec<PurchaseOrderDetail>, sqlx::Error> {
        sqlx::query_as::<_, PurchaseOrderDetail>(
            r#"SELECT * FROM "発注データ明細" WHERE "発注番号" = $1 ORDER BY "発注行番号""#,
        )
        .bind(po_no)
        .fetch_all(pool)
        .await
    }

    /// 発注明細を更新
    pub async fn update(pool: &PgPool, detail: &PurchaseOrderDetail) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "発注データ明細" SET
                "商品コード" = $3,
                "商品名" = $4,
                "発注数量" = $5,
                "単価" = $6,
                "入荷予定数量" = $7,
                "入荷済数量" = $8,
                "完了フラグ" = $9,
                "更新日時" = $10,
                "更新者名" = $11
            WHERE "発注番号" = $1 AND "発注行番号" = $2
            "#,
        )
        .bind(&detail.po_no)
        .bind(detail.po_row_no)
        .bind(&detail.prod_code)
        .bind(&detail.prod_name)
        .bind(detail.quantity)
        .bind(detail.unit_price)
        .bind(detail.expected_qty)
        .bind(detail.received_qty)
        .bind(detail.complete_flg)
        .bind(detail.update_date)
        .bind(&detail.updater)
        .execute(pool)
        .await?;
        Ok(())
    }

    /// 発注明細を削除
    pub async fn delete(pool: &PgPool, po_no: &str, po_row_no: i32) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "発注データ明細" WHERE "発注番号" = $1 AND "発注行番号" = $2"#)
            .bind(po_no)
            .bind(po_row_no)
            .execute(pool)
            .await?;
        Ok(())
    }

    /// 全ての発注明細を削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "発注データ明細""#).execute(pool).await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::repository::{
        DepartmentRepository, EmployeeRepository, ProductCategoryRepository, ProductRepository,
        WarehouseRepository,
    };
    use crate::test_support::with_test_pool;
    use chrono::NaiveDate;

    async fn setup(pool: &PgPool) {
        // テーブルをクリーンアップ（依存関係の逆順）
        PurchaseOrderDetailRepository::delete_all(pool).await.ok();
        PurchaseOrderRepository::delete_all(pool).await.ok();
        WarehouseRepository::delete_all(pool).await.ok();
        EmployeeRepository::delete_all(pool).await.ok();
        DepartmentRepository::delete_all(pool).await.ok();
        ProductRepository::delete_all(pool).await.ok();
        ProductCategoryRepository::delete_all(pool).await.ok();

        // 仕入先マスタの前提データを作成
        sqlx::query(
            r#"INSERT INTO "取引先グループマスタ" (
                "取引先グループコード", "取引先グループ名",
                "作成日時", "更新日時"
            ) VALUES ('GRP1', 'テストグループ', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .ok();

        sqlx::query(
            r#"INSERT INTO "取引先マスタ" (
                "取引先コード", "取引先名", "取引先名カナ",
                "取引先グループコード",
                "作成日時", "更新日時"
            ) VALUES ('COMP001', 'テスト取引先', 'テストトリヒキサキ', 'GRP1', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .ok();

        sqlx::query(
            r#"INSERT INTO "仕入先マスタ" (
                 "仕入先コード", "仕入先枝番", "仕入先名", "仕入先締日", "仕入先支払月", "仕入先支払日",
                 "作成日時", "更新日時"
               ) VALUES ('COMP001', 1, 'テスト仕入先', 25, 1, 10, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .expect("Failed to insert supplier");

        // 部門、社員、倉庫を登録
        sqlx::query(
            r#"INSERT INTO "部門マスタ" (
                "部門コード", "開始日", "部門名", "組織階層", "部門パス", "最下層区分", "伝票入力可否",
                "作成日時", "更新日時"
            ) VALUES ('D001', '2021-01-01', 'テスト部門', 1, '/D001', 1, 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .expect("Failed to insert department");

        sqlx::query(
            r#"INSERT INTO "社員マスタ" (
                "社員コード", "社員名", "社員名カナ",
                "部門コード", "開始日", "職種コード", "承認権限コード",
                "作成日時", "更新日時"
            ) VALUES ('EMP999', 'テスト社員', 'テストシャイン', 'D001', '2021-01-01', '01', '01', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .expect("Failed to insert employee");

        sqlx::query(
            r#"INSERT INTO "倉庫マスタ" (
                "倉庫コード", "倉庫名",
                "作成日時", "更新日時"
            ) VALUES ('WH1', '東京倉庫', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .expect("Failed to insert warehouse");

        // 商品データを登録
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
    }

    fn create_test_purchase_order() -> PurchaseOrder {
        PurchaseOrder {
            po_no: "PO0000001".to_string(),
            po_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 5).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            ),
            sup_code: "COMP001".to_string(),
            sup_sub_no: Some(1),
            emp_code: "EMP999".to_string(),
            due_date: Some(
                NaiveDate::from_ymd_opt(2021, 1, 15).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            ),
            wh_code: "WH1".to_string(),
            po_amount: Some(50000),
            cmp_tax: 5000,
            slip_comment: Some("テスト発注".to_string()),
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    fn create_test_purchase_order_detail() -> PurchaseOrderDetail {
        PurchaseOrderDetail {
            po_no: "PO0000001".to_string(),
            po_row_no: 1,
            prod_code: "PROD001".to_string(),
            prod_name: Some("テスト商品".to_string()),
            quantity: 100,
            unit_price: Some(500),
            expected_qty: Some(100),
            received_qty: Some(0),
            complete_flg: 0,
            create_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            creator: Some("admin".to_string()),
            update_date: NaiveDate::from_ymd_opt(2021, 1, 1).unwrap().and_hms_opt(0, 0, 0).unwrap(),
            updater: Some("admin".to_string()),
        }
    }

    #[tokio::test]
    async fn test_purchase_order_create_and_find() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let po = create_test_purchase_order();

            // 発注を登録
            PurchaseOrderRepository::create(&pool, &po)
                .await
                .expect("Failed to create purchase order");

            // 登録した発注を取得
            let found = PurchaseOrderRepository::find_by_po_no(&pool, &po.po_no)
                .await
                .expect("Failed to find purchase order");

            assert_eq!(found.po_no, po.po_no);
            assert_eq!(found.sup_code, po.sup_code);
            assert_eq!(found.po_amount, po.po_amount);
        })
        .await;
    }

    #[tokio::test]
    async fn test_purchase_order_update() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let mut po = create_test_purchase_order();

            // 発注を登録
            PurchaseOrderRepository::create(&pool, &po)
                .await
                .expect("Failed to create purchase order");

            // 発注金額を更新
            po.po_amount = Some(60000);
            PurchaseOrderRepository::update(&pool, &po)
                .await
                .expect("Failed to update purchase order");

            // 更新された発注を取得
            let updated = PurchaseOrderRepository::find_by_po_no(&pool, &po.po_no)
                .await
                .expect("Failed to find purchase order");

            assert_eq!(updated.po_amount, Some(60000));
        })
        .await;
    }

    #[tokio::test]
    async fn test_purchase_order_delete() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let po = create_test_purchase_order();

            // 発注を登録
            PurchaseOrderRepository::create(&pool, &po)
                .await
                .expect("Failed to create purchase order");

            // 発注を削除
            PurchaseOrderRepository::delete(&pool, &po.po_no)
                .await
                .expect("Failed to delete purchase order");

            // 削除された発注を取得しようとするとエラー
            let result = PurchaseOrderRepository::find_by_po_no(&pool, &po.po_no).await;
            assert!(result.is_err());
        })
        .await;
    }

    #[tokio::test]
    async fn test_purchase_order_detail_create_and_find() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let po = create_test_purchase_order();
            let detail = create_test_purchase_order_detail();

            // 発注を登録
            PurchaseOrderRepository::create(&pool, &po)
                .await
                .expect("Failed to create purchase order");

            // 発注明細を登録
            PurchaseOrderDetailRepository::create(&pool, &detail)
                .await
                .expect("Failed to create purchase order detail");

            // 登録した発注明細を取得
            let details = PurchaseOrderDetailRepository::find_by_po_no(&pool, &detail.po_no)
                .await
                .expect("Failed to find purchase order details");

            assert_eq!(details.len(), 1);
            assert_eq!(details[0].po_no, detail.po_no);
            assert_eq!(details[0].prod_code, detail.prod_code);
            assert_eq!(details[0].quantity, detail.quantity);
        })
        .await;
    }

    #[tokio::test]
    async fn test_purchase_order_detail_update() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let po = create_test_purchase_order();
            let mut detail = create_test_purchase_order_detail();

            // 発注と発注明細を登録
            PurchaseOrderRepository::create(&pool, &po)
                .await
                .expect("Failed to create purchase order");
            PurchaseOrderDetailRepository::create(&pool, &detail)
                .await
                .expect("Failed to create purchase order detail");

            // 入荷済数量を更新
            detail.received_qty = Some(50);
            PurchaseOrderDetailRepository::update(&pool, &detail)
                .await
                .expect("Failed to update purchase order detail");

            // 更新された発注明細を取得
            let details = PurchaseOrderDetailRepository::find_by_po_no(&pool, &detail.po_no)
                .await
                .expect("Failed to find purchase order details");

            assert_eq!(details[0].received_qty, Some(50));
        })
        .await;
    }

    #[tokio::test]
    async fn test_purchase_order_detail_delete() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let po = create_test_purchase_order();
            let detail = create_test_purchase_order_detail();

            // 発注と発注明細を登録
            PurchaseOrderRepository::create(&pool, &po)
                .await
                .expect("Failed to create purchase order");
            PurchaseOrderDetailRepository::create(&pool, &detail)
                .await
                .expect("Failed to create purchase order detail");

            // 発注明細を削除
            PurchaseOrderDetailRepository::delete(&pool, &detail.po_no, detail.po_row_no)
                .await
                .expect("Failed to delete purchase order detail");

            // 削除された発注明細を取得しようとすると空になる
            let details = PurchaseOrderDetailRepository::find_by_po_no(&pool, &detail.po_no)
                .await
                .expect("Failed to find purchase order details");
            assert_eq!(details.len(), 0);
        })
        .await;
    }
}
