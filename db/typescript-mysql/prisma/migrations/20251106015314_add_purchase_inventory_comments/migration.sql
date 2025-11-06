-- Add comments to purchase and inventory tables

SET NAMES utf8mb4;

-- warehouse table
ALTER TABLE `warehouse` COMMENT = '倉庫マスタテーブル';
ALTER TABLE `warehouse` MODIFY `wh_code` VARCHAR(3) NOT NULL COMMENT '倉庫コード';
ALTER TABLE `warehouse` MODIFY `name` VARCHAR(40) NOT NULL COMMENT '倉庫名';
ALTER TABLE `warehouse` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `warehouse` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `warehouse` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `warehouse` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- purchase_order table
ALTER TABLE `purchase_order` COMMENT = '発注データテーブル';
ALTER TABLE `purchase_order` MODIFY `po_no` VARCHAR(10) NOT NULL COMMENT '発注番号';
ALTER TABLE `purchase_order` MODIFY `po_date` DATETIME(6) NULL COMMENT '発注日';
ALTER TABLE `purchase_order` MODIFY `order_no` VARCHAR(10) NOT NULL COMMENT '受注番号';
ALTER TABLE `purchase_order` MODIFY `sup_code` VARCHAR(8) NOT NULL COMMENT '仕入先コード';
ALTER TABLE `purchase_order` MODIFY `sup_sub_no` INTEGER NULL COMMENT '仕入先枝番';
ALTER TABLE `purchase_order` MODIFY `emp_code` VARCHAR(10) NOT NULL COMMENT '発注担当者コード';
ALTER TABLE `purchase_order` MODIFY `due_date` DATETIME(6) NULL COMMENT '指定納期';
ALTER TABLE `purchase_order` MODIFY `wh_code` VARCHAR(3) NOT NULL COMMENT '倉庫コード';
ALTER TABLE `purchase_order` MODIFY `po_amnt` INTEGER NULL COMMENT '発注金額合計';
ALTER TABLE `purchase_order` MODIFY `cmp_tax` INTEGER NOT NULL COMMENT '消費税合計';
ALTER TABLE `purchase_order` MODIFY `slip_comment` VARCHAR(1000) NULL COMMENT '備考';
ALTER TABLE `purchase_order` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `purchase_order` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `purchase_order` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `purchase_order` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- purchase_order_detail table
ALTER TABLE `purchase_order_detail` COMMENT = '発注明細テーブル';
ALTER TABLE `purchase_order_detail` MODIFY `po_no` VARCHAR(10) NOT NULL COMMENT '発注番号';
ALTER TABLE `purchase_order_detail` MODIFY `po_row_no` INTEGER NOT NULL COMMENT '発注行番号';
ALTER TABLE `purchase_order_detail` MODIFY `prod_code` VARCHAR(16) NOT NULL COMMENT '商品コード';
ALTER TABLE `purchase_order_detail` MODIFY `prod_name` VARCHAR(10) NOT NULL COMMENT '商品名';
ALTER TABLE `purchase_order_detail` MODIFY `unitprice` INTEGER NOT NULL COMMENT '発注単価';
ALTER TABLE `purchase_order_detail` MODIFY `quantity` INTEGER NOT NULL COMMENT '発注数量';
ALTER TABLE `purchase_order_detail` MODIFY `cmp_tax_rate` INTEGER NULL COMMENT '消費税率';
ALTER TABLE `purchase_order_detail` MODIFY `received_qty` INTEGER NULL COMMENT '入荷数量';
ALTER TABLE `purchase_order_detail` MODIFY `complete_flg` INTEGER NOT NULL COMMENT '完了フラグ';
ALTER TABLE `purchase_order_detail` MODIFY `discount` INTEGER NOT NULL COMMENT '値引金額';
ALTER TABLE `purchase_order_detail` MODIFY `delivery_date` DATETIME(6) NULL COMMENT '納期';
ALTER TABLE `purchase_order_detail` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `purchase_order_detail` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `purchase_order_detail` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `purchase_order_detail` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- purchase table
ALTER TABLE `purchase` COMMENT = '仕入データテーブル';
ALTER TABLE `purchase` MODIFY `pu_no` VARCHAR(10) NOT NULL COMMENT '仕入番号';
ALTER TABLE `purchase` MODIFY `pu_date` DATETIME(6) NULL COMMENT '仕入日';
ALTER TABLE `purchase` MODIFY `sup_code` VARCHAR(8) NOT NULL COMMENT '仕入先コード';
ALTER TABLE `purchase` MODIFY `sup_sub_no` INTEGER NULL COMMENT '仕入先枝番';
ALTER TABLE `purchase` MODIFY `emp_code` VARCHAR(10) NOT NULL COMMENT '仕入担当者コード';
ALTER TABLE `purchase` MODIFY `start_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '開始日';
ALTER TABLE `purchase` MODIFY `po_no` VARCHAR(10) NULL COMMENT '発注番号';
ALTER TABLE `purchase` MODIFY `dept_code` VARCHAR(6) NOT NULL COMMENT '部門コード';
ALTER TABLE `purchase` MODIFY `pu_ammount` INTEGER NULL COMMENT '仕入金額合計';
ALTER TABLE `purchase` MODIFY `cmp_tax` INTEGER NOT NULL COMMENT '消費税合計';
ALTER TABLE `purchase` MODIFY `slip_comment` VARCHAR(1000) NULL COMMENT '備考';
ALTER TABLE `purchase` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `purchase` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `purchase` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `purchase` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- purchase_detail table
ALTER TABLE `purchase_detail` COMMENT = '仕入明細テーブル';
ALTER TABLE `purchase_detail` MODIFY `pu_no` VARCHAR(10) NOT NULL COMMENT '仕入番号';
ALTER TABLE `purchase_detail` MODIFY `pu_row_no` INTEGER NOT NULL COMMENT '仕入行番号';
ALTER TABLE `purchase_detail` MODIFY `prod_code` VARCHAR(16) NOT NULL COMMENT '商品コード';
ALTER TABLE `purchase_detail` MODIFY `prod_name` VARCHAR(10) NOT NULL COMMENT '商品名';
ALTER TABLE `purchase_detail` MODIFY `unitprice` INTEGER NOT NULL COMMENT '仕入単価';
ALTER TABLE `purchase_detail` MODIFY `quantity` INTEGER NOT NULL COMMENT '仕入数量';
ALTER TABLE `purchase_detail` MODIFY `cmp_tax_rate` INTEGER NULL COMMENT '消費税率';
ALTER TABLE `purchase_detail` MODIFY `discount` INTEGER NOT NULL COMMENT '値引金額';
ALTER TABLE `purchase_detail` MODIFY `rot_no` VARCHAR(20) NOT NULL COMMENT 'ロット番号';
ALTER TABLE `purchase_detail` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `purchase_detail` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `purchase_detail` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `purchase_detail` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- stock table
ALTER TABLE `stock` COMMENT = '在庫データテーブル';
ALTER TABLE `stock` MODIFY `wh_code` VARCHAR(3) NOT NULL COMMENT '倉庫コード';
ALTER TABLE `stock` MODIFY `prod_code` VARCHAR(16) NOT NULL COMMENT '商品コード';
ALTER TABLE `stock` MODIFY `rot_no` VARCHAR(20) NOT NULL COMMENT 'ロット番号';
ALTER TABLE `stock` MODIFY `stock_type` VARCHAR(1) NOT NULL COMMENT '在庫区分';
ALTER TABLE `stock` MODIFY `quality_type` VARCHAR(1) NOT NULL COMMENT '良品区分';
ALTER TABLE `stock` MODIFY `actual` INTEGER NOT NULL COMMENT '実在庫数';
ALTER TABLE `stock` MODIFY `valid` INTEGER NOT NULL COMMENT '有効在庫数';
ALTER TABLE `stock` MODIFY `last_delivery_date` DATETIME(6) NULL COMMENT '最終出荷日';
ALTER TABLE `stock` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `stock` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `stock` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `stock` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';
