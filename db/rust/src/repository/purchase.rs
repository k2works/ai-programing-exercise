use crate::entity::purchase::{Purchase, PurchaseDetail};
use sqlx::PgPool;

/// 仕入データのリポジトリ
pub struct PurchaseRepository;

impl PurchaseRepository {
    /// 仕入データを作成
    pub async fn create(pool: &PgPool, pu: &Purchase) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "仕入データ" (
                "仕入番号", "仕入日", "仕入先コード", "仕入先枝番",
                "仕入担当者コード", "開始日", "発注番号", "部門コード",
                "仕入金額合計", "消費税合計", "備考",
                "作成日時", "作成者名", "更新日時", "更新者名"
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14, $15)
            "#,
        )
        .bind(&pu.pu_no)
        .bind(pu.pu_date)
        .bind(&pu.sup_code)
        .bind(pu.sup_sub_no)
        .bind(&pu.emp_code)
        .bind(pu.start_date)
        .bind(&pu.po_no)
        .bind(&pu.dept_code)
        .bind(pu.pu_amount)
        .bind(pu.cmp_tax)
        .bind(&pu.slip_comment)
        .bind(pu.create_date)
        .bind(&pu.creator)
        .bind(pu.update_date)
        .bind(&pu.updater)
        .execute(pool)
        .await?;

        Ok(())
    }

    /// 仕入番号で仕入データを取得
    pub async fn find_by_pu_no(pool: &PgPool, pu_no: &str) -> Result<Purchase, sqlx::Error> {
        sqlx::query_as::<_, Purchase>(
            r#"
            SELECT
                "仕入番号", "仕入日", "仕入先コード", "仕入先枝番",
                "仕入担当者コード", "開始日", "発注番号", "部門コード",
                "仕入金額合計", "消費税合計", "備考",
                "作成日時", "作成者名", "更新日時", "更新者名"
            FROM "仕入データ"
            WHERE "仕入番号" = $1
            "#,
        )
        .bind(pu_no)
        .fetch_one(pool)
        .await
    }

    /// 仕入データを更新
    pub async fn update(pool: &PgPool, pu: &Purchase) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "仕入データ"
            SET
                "仕入日" = $2,
                "仕入先コード" = $3,
                "仕入先枝番" = $4,
                "仕入担当者コード" = $5,
                "開始日" = $6,
                "発注番号" = $7,
                "部門コード" = $8,
                "仕入金額合計" = $9,
                "消費税合計" = $10,
                "備考" = $11,
                "更新日時" = $12,
                "更新者名" = $13
            WHERE "仕入番号" = $1
            "#,
        )
        .bind(&pu.pu_no)
        .bind(pu.pu_date)
        .bind(&pu.sup_code)
        .bind(pu.sup_sub_no)
        .bind(&pu.emp_code)
        .bind(pu.start_date)
        .bind(&pu.po_no)
        .bind(&pu.dept_code)
        .bind(pu.pu_amount)
        .bind(pu.cmp_tax)
        .bind(&pu.slip_comment)
        .bind(pu.update_date)
        .bind(&pu.updater)
        .execute(pool)
        .await?;

        Ok(())
    }

    /// 仕入データを削除
    pub async fn delete(pool: &PgPool, pu_no: &str) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            DELETE FROM "仕入データ"
            WHERE "仕入番号" = $1
            "#,
        )
        .bind(pu_no)
        .execute(pool)
        .await?;

        Ok(())
    }

    /// 全ての仕入データを削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "仕入データ""#).execute(pool).await?;
        Ok(())
    }
}

/// 仕入データ明細のリポジトリ
pub struct PurchaseDetailRepository;

impl PurchaseDetailRepository {
    /// 仕入データ明細を作成
    pub async fn create(pool: &PgPool, detail: &PurchaseDetail) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            INSERT INTO "仕入データ明細" (
                "仕入番号", "仕入行番号", "商品コード", "商品名",
                "仕入数量", "単価", "倉庫コード", "ロット番号",
                "発注番号", "発注行番号",
                "作成日時", "作成者名", "更新日時", "更新者名"
            )
            VALUES ($1, $2, $3, $4, $5, $6, $7, $8, $9, $10, $11, $12, $13, $14)
            "#,
        )
        .bind(&detail.pu_no)
        .bind(detail.pu_row_no)
        .bind(&detail.prod_code)
        .bind(&detail.prod_name)
        .bind(detail.quantity)
        .bind(detail.unit_price)
        .bind(&detail.wh_code)
        .bind(&detail.lot_no)
        .bind(&detail.po_no)
        .bind(detail.po_row_no)
        .bind(detail.create_date)
        .bind(&detail.creator)
        .bind(detail.update_date)
        .bind(&detail.updater)
        .execute(pool)
        .await?;

        Ok(())
    }

    /// 仕入番号で仕入データ明細を取得
    pub async fn find_by_pu_no(
        pool: &PgPool,
        pu_no: &str,
    ) -> Result<Vec<PurchaseDetail>, sqlx::Error> {
        sqlx::query_as::<_, PurchaseDetail>(
            r#"
            SELECT
                "仕入番号", "仕入行番号", "商品コード", "商品名",
                "仕入数量", "単価", "倉庫コード", "ロット番号",
                "発注番号", "発注行番号",
                "作成日時", "作成者名", "更新日時", "更新者名"
            FROM "仕入データ明細"
            WHERE "仕入番号" = $1
            ORDER BY "仕入行番号"
            "#,
        )
        .bind(pu_no)
        .fetch_all(pool)
        .await
    }

    /// 仕入データ明細を更新
    pub async fn update(pool: &PgPool, detail: &PurchaseDetail) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            UPDATE "仕入データ明細"
            SET
                "商品コード" = $3,
                "商品名" = $4,
                "仕入数量" = $5,
                "単価" = $6,
                "倉庫コード" = $7,
                "ロット番号" = $8,
                "発注番号" = $9,
                "発注行番号" = $10,
                "更新日時" = $11,
                "更新者名" = $12
            WHERE "仕入番号" = $1 AND "仕入行番号" = $2
            "#,
        )
        .bind(&detail.pu_no)
        .bind(detail.pu_row_no)
        .bind(&detail.prod_code)
        .bind(&detail.prod_name)
        .bind(detail.quantity)
        .bind(detail.unit_price)
        .bind(&detail.wh_code)
        .bind(&detail.lot_no)
        .bind(&detail.po_no)
        .bind(detail.po_row_no)
        .bind(detail.update_date)
        .bind(&detail.updater)
        .execute(pool)
        .await?;

        Ok(())
    }

    /// 仕入データ明細を削除
    pub async fn delete(pool: &PgPool, pu_no: &str, pu_row_no: i32) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            DELETE FROM "仕入データ明細"
            WHERE "仕入番号" = $1 AND "仕入行番号" = $2
            "#,
        )
        .bind(pu_no)
        .bind(pu_row_no)
        .execute(pool)
        .await?;

        Ok(())
    }

    /// 全ての仕入データ明細を削除（テスト用）
    pub async fn delete_all(pool: &PgPool) -> Result<(), sqlx::Error> {
        sqlx::query(r#"DELETE FROM "仕入データ明細""#).execute(pool).await?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_support::with_test_pool;
    use chrono::NaiveDate;

    async fn setup(pool: &PgPool) {
        // クリーンアップ（依存関係の逆順）
        PurchaseDetailRepository::delete_all(pool).await.ok();
        PurchaseRepository::delete_all(pool).await.ok();

        // Delete prerequisite data (in reverse dependency order)
        sqlx::query(r#"DELETE FROM "商品マスタ" WHERE "商品コード" = 'PROD001'"#)
            .execute(pool)
            .await
            .ok();
        sqlx::query(r#"DELETE FROM "倉庫マスタ" WHERE "倉庫コード" = 'WH1'"#)
            .execute(pool)
            .await
            .ok();
        sqlx::query(r#"DELETE FROM "仕入先マスタ" WHERE "仕入先コード" = 'COMP001'"#)
            .execute(pool)
            .await
            .ok();
        sqlx::query(r#"DELETE FROM "社員マスタ" WHERE "社員コード" = 'EMP999'"#)
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

        // 3. 仕入先マスタ
        sqlx::query(
            r#"INSERT INTO "仕入先マスタ" (
                 "仕入先コード", "仕入先枝番", "仕入先名", "仕入先締日", "仕入先支払月", "仕入先支払日",
                 "作成日時", "更新日時"
               ) VALUES ('COMP001', 1, 'テスト仕入先', 25, 1, 10, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#
        ).execute(pool).await.expect("Failed to insert supplier");

        // 4. 部門マスタ
        sqlx::query(
            r#"INSERT INTO "部門マスタ" (
                 "部門コード", "開始日", "部門名", "組織階層", "部門パス", "最下層区分", "伝票入力可否",
                 "作成日時", "更新日時"
               ) VALUES ('D001', '2021-01-01', 'テスト部門', 1, '/D001', 1, 1, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#
        ).execute(pool).await.expect("Failed to insert department");

        // 5. 社員マスタ
        sqlx::query(
            r#"INSERT INTO "社員マスタ" (
                 "社員コード", "社員名", "部門コード", "開始日", "職種コード", "承認権限コード",
                 "作成日時", "更新日時"
               ) VALUES ('EMP999', 'テスト社員', 'D001', '2021-01-01', '01', '01', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#
        ).execute(pool).await.expect("Failed to insert employee");

        // 6. 倉庫マスタ
        sqlx::query(
            r#"INSERT INTO "倉庫マスタ" (
                 "倉庫コード", "倉庫名", "作成日時", "更新日時"
               ) VALUES ('WH1', '本社倉庫', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#,
        )
        .execute(pool)
        .await
        .expect("Failed to insert warehouse");

        // 7. 商品マスタ
        sqlx::query(
            r#"INSERT INTO "商品マスタ" (
                 "商品コード", "商品正式名", "商品略称", "商品名カナ", "仕入先コード",
                 "作成日時", "更新日時"
               ) VALUES ('PROD001', 'テスト商品', 'テスト', 'テストショウヒン', 'COMP001', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP)"#
        ).execute(pool).await.expect("Failed to insert product");
    }

    #[tokio::test]
    async fn test_purchase_create_and_find() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let pu = Purchase {
                pu_no: "PU0000001".to_string(),
                pu_date: Some(
                    NaiveDate::from_ymd_opt(2021, 1, 10).unwrap().and_hms_opt(0, 0, 0).unwrap(),
                ),
                sup_code: "COMP001".to_string(),
                sup_sub_no: Some(1),
                emp_code: "EMP999".to_string(),
                start_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
                po_no: Some("PO0000001".to_string()),
                dept_code: "D001".to_string(),
                pu_amount: Some(50000),
                cmp_tax: 5000,
                slip_comment: Some("テスト仕入".to_string()),
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
            };

            PurchaseRepository::create(&pool, &pu).await.unwrap();

            let found = PurchaseRepository::find_by_pu_no(&pool, "PU0000001").await.unwrap();

            assert_eq!(found.pu_no, pu.pu_no);
            assert_eq!(found.sup_code, pu.sup_code);
            assert_eq!(found.pu_amount, pu.pu_amount);
        })
        .await;
    }

    #[tokio::test]
    async fn test_purchase_update() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let mut pu = Purchase {
                pu_no: "PU0000001".to_string(),
                pu_date: Some(
                    NaiveDate::from_ymd_opt(2021, 1, 10).unwrap().and_hms_opt(0, 0, 0).unwrap(),
                ),
                sup_code: "COMP001".to_string(),
                sup_sub_no: Some(1),
                emp_code: "EMP999".to_string(),
                start_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
                po_no: Some("PO0000001".to_string()),
                dept_code: "D001".to_string(),
                pu_amount: Some(50000),
                cmp_tax: 5000,
                slip_comment: Some("テスト仕入".to_string()),
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
            };

            PurchaseRepository::create(&pool, &pu).await.unwrap();

            pu.pu_amount = Some(60000);
            pu.cmp_tax = 6000;
            pu.updater = Some("updater".to_string());

            PurchaseRepository::update(&pool, &pu).await.unwrap();

            let found = PurchaseRepository::find_by_pu_no(&pool, "PU0000001").await.unwrap();

            assert_eq!(found.pu_amount, Some(60000));
            assert_eq!(found.cmp_tax, 6000);
            assert_eq!(found.updater, Some("updater".to_string()));
        })
        .await;
    }

    #[tokio::test]
    async fn test_purchase_delete() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            let pu = Purchase {
                pu_no: "PU0000001".to_string(),
                pu_date: Some(
                    NaiveDate::from_ymd_opt(2021, 1, 10).unwrap().and_hms_opt(0, 0, 0).unwrap(),
                ),
                sup_code: "COMP001".to_string(),
                sup_sub_no: Some(1),
                emp_code: "EMP999".to_string(),
                start_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
                po_no: Some("PO0000001".to_string()),
                dept_code: "D001".to_string(),
                pu_amount: Some(50000),
                cmp_tax: 5000,
                slip_comment: Some("テスト仕入".to_string()),
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
            };

            PurchaseRepository::create(&pool, &pu).await.unwrap();
            PurchaseRepository::delete(&pool, "PU0000001").await.unwrap();

            let result = PurchaseRepository::find_by_pu_no(&pool, "PU0000001").await;
            assert!(result.is_err());
        })
        .await;
    }

    #[tokio::test]
    async fn test_purchase_detail_create_and_find() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // 親の仕入データを作成
            let pu = Purchase {
                pu_no: "PU0000001".to_string(),
                pu_date: Some(
                    NaiveDate::from_ymd_opt(2021, 1, 10).unwrap().and_hms_opt(0, 0, 0).unwrap(),
                ),
                sup_code: "COMP001".to_string(),
                sup_sub_no: Some(1),
                emp_code: "EMP999".to_string(),
                start_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
                po_no: Some("PO0000001".to_string()),
                dept_code: "D001".to_string(),
                pu_amount: Some(50000),
                cmp_tax: 5000,
                slip_comment: Some("テスト仕入".to_string()),
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
            };
            PurchaseRepository::create(&pool, &pu).await.unwrap();

            let detail = PurchaseDetail {
                pu_no: "PU0000001".to_string(),
                pu_row_no: 1,
                prod_code: "PROD001".to_string(),
                prod_name: Some("テスト商品".to_string()),
                quantity: 100,
                unit_price: Some(500),
                wh_code: "WH1".to_string(),
                lot_no: "LOT20210110".to_string(),
                po_no: Some("PO0000001".to_string()),
                po_row_no: Some(1),
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
            };

            PurchaseDetailRepository::create(&pool, &detail).await.unwrap();

            let details =
                PurchaseDetailRepository::find_by_pu_no(&pool, "PU0000001").await.unwrap();

            assert_eq!(details.len(), 1);
            assert_eq!(details[0].pu_no, detail.pu_no);
            assert_eq!(details[0].prod_code, detail.prod_code);
            assert_eq!(details[0].quantity, detail.quantity);
        })
        .await;
    }

    #[tokio::test]
    async fn test_purchase_detail_update() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // 親の仕入データを作成
            let pu = Purchase {
                pu_no: "PU0000001".to_string(),
                pu_date: Some(
                    NaiveDate::from_ymd_opt(2021, 1, 10).unwrap().and_hms_opt(0, 0, 0).unwrap(),
                ),
                sup_code: "COMP001".to_string(),
                sup_sub_no: Some(1),
                emp_code: "EMP999".to_string(),
                start_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
                po_no: Some("PO0000001".to_string()),
                dept_code: "D001".to_string(),
                pu_amount: Some(50000),
                cmp_tax: 5000,
                slip_comment: Some("テスト仕入".to_string()),
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
            };
            PurchaseRepository::create(&pool, &pu).await.unwrap();

            let mut detail = PurchaseDetail {
                pu_no: "PU0000001".to_string(),
                pu_row_no: 1,
                prod_code: "PROD001".to_string(),
                prod_name: Some("テスト商品".to_string()),
                quantity: 100,
                unit_price: Some(500),
                wh_code: "WH1".to_string(),
                lot_no: "LOT20210110".to_string(),
                po_no: Some("PO0000001".to_string()),
                po_row_no: Some(1),
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
            };

            PurchaseDetailRepository::create(&pool, &detail).await.unwrap();

            detail.quantity = 200;
            detail.updater = Some("updater".to_string());

            PurchaseDetailRepository::update(&pool, &detail).await.unwrap();

            let details =
                PurchaseDetailRepository::find_by_pu_no(&pool, "PU0000001").await.unwrap();

            assert_eq!(details[0].quantity, 200);
            assert_eq!(details[0].updater, Some("updater".to_string()));
        })
        .await;
    }

    #[tokio::test]
    async fn test_purchase_detail_delete() {
        with_test_pool(|pool| async move {
            setup(&pool).await;

            // 親の仕入データを作成
            let pu = Purchase {
                pu_no: "PU0000001".to_string(),
                pu_date: Some(
                    NaiveDate::from_ymd_opt(2021, 1, 10).unwrap().and_hms_opt(0, 0, 0).unwrap(),
                ),
                sup_code: "COMP001".to_string(),
                sup_sub_no: Some(1),
                emp_code: "EMP999".to_string(),
                start_date: NaiveDate::from_ymd_opt(2021, 1, 1)
                    .unwrap()
                    .and_hms_opt(0, 0, 0)
                    .unwrap(),
                po_no: Some("PO0000001".to_string()),
                dept_code: "D001".to_string(),
                pu_amount: Some(50000),
                cmp_tax: 5000,
                slip_comment: Some("テスト仕入".to_string()),
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
            };
            PurchaseRepository::create(&pool, &pu).await.unwrap();

            let detail = PurchaseDetail {
                pu_no: "PU0000001".to_string(),
                pu_row_no: 1,
                prod_code: "PROD001".to_string(),
                prod_name: Some("テスト商品".to_string()),
                quantity: 100,
                unit_price: Some(500),
                wh_code: "WH1".to_string(),
                lot_no: "LOT20210110".to_string(),
                po_no: Some("PO0000001".to_string()),
                po_row_no: Some(1),
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
            };

            PurchaseDetailRepository::create(&pool, &detail).await.unwrap();

            PurchaseDetailRepository::delete(&pool, "PU0000001", 1).await.unwrap();

            let details =
                PurchaseDetailRepository::find_by_pu_no(&pool, "PU0000001").await.unwrap();

            assert_eq!(details.len(), 0);
        })
        .await;
    }
}
