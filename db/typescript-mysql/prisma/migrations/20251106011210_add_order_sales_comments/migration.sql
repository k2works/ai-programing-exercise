-- Add comments to order and sales tables

-- order table
ALTER TABLE `order` COMMENT = '受注データテーブル';
ALTER TABLE `order` MODIFY `order_no` VARCHAR(10) NOT NULL COMMENT '受注番号';
ALTER TABLE `order` MODIFY `order_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '受注日';
ALTER TABLE `order` MODIFY `dept_code` VARCHAR(6) NOT NULL COMMENT '部門コード';
ALTER TABLE `order` MODIFY `start_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '部門開始日';
ALTER TABLE `order` MODIFY `cust_code` VARCHAR(8) NOT NULL COMMENT '顧客コード';
ALTER TABLE `order` MODIFY `cust_sub_no` INTEGER NULL COMMENT '顧客枝番';
ALTER TABLE `order` MODIFY `emp_code` VARCHAR(10) NOT NULL COMMENT '社員コード';
ALTER TABLE `order` MODIFY `required_date` DATETIME(6) NULL COMMENT '希望納期';
ALTER TABLE `order` MODIFY `custorder_no` VARCHAR(20) NULL COMMENT '客先注文番号';
ALTER TABLE `order` MODIFY `wh_code` VARCHAR(3) NOT NULL COMMENT '倉庫コード';
ALTER TABLE `order` MODIFY `order_amnt` INTEGER NOT NULL COMMENT '受注金額合計';
ALTER TABLE `order` MODIFY `cmp_tax` INTEGER NOT NULL COMMENT '消費税合計';
ALTER TABLE `order` MODIFY `slip_comment` VARCHAR(1000) NULL COMMENT '備考';
ALTER TABLE `order` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `order` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `order` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `order` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- order_detail table
ALTER TABLE `order_detail` COMMENT = '受注明細テーブル';
ALTER TABLE `order_detail` MODIFY `order_no` VARCHAR(10) NOT NULL COMMENT '受注番号';
ALTER TABLE `order_detail` MODIFY `so_row_no` INTEGER NOT NULL COMMENT '受注行番号';
ALTER TABLE `order_detail` MODIFY `prod_code` VARCHAR(16) NOT NULL COMMENT '商品コード';
ALTER TABLE `order_detail` MODIFY `prod_name` VARCHAR(10) NOT NULL COMMENT '商品名';
ALTER TABLE `order_detail` MODIFY `unitprice` INTEGER NOT NULL COMMENT '販売単価';
ALTER TABLE `order_detail` MODIFY `quantity` INTEGER NOT NULL COMMENT '受注数量';
ALTER TABLE `order_detail` MODIFY `cmp_tax_rate` INTEGER NULL COMMENT '消費税率';
ALTER TABLE `order_detail` MODIFY `reserve_qty` INTEGER NULL COMMENT '引当数量';
ALTER TABLE `order_detail` MODIFY `delivery_order_qty` INTEGER NULL COMMENT '出荷指示数量';
ALTER TABLE `order_detail` MODIFY `delivered_qty` INTEGER NULL COMMENT '出荷済数量';
ALTER TABLE `order_detail` MODIFY `complete_flg` INTEGER NOT NULL COMMENT '完了フラグ';
ALTER TABLE `order_detail` MODIFY `discount` INTEGER NOT NULL COMMENT '値引金額';
ALTER TABLE `order_detail` MODIFY `delivery_date` DATETIME(6) NULL COMMENT '納期';
ALTER TABLE `order_detail` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `order_detail` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `order_detail` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `order_detail` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- sales table
ALTER TABLE `sales` COMMENT = '売上データテーブル';
ALTER TABLE `sales` MODIFY `sales_no` VARCHAR(10) NOT NULL COMMENT '売上番号';
ALTER TABLE `sales` MODIFY `order_no` VARCHAR(10) NOT NULL COMMENT '受注番号';
ALTER TABLE `sales` MODIFY `sales_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '売上日';
ALTER TABLE `sales` MODIFY `sales_type` INTEGER NULL COMMENT '売上区分';
ALTER TABLE `sales` MODIFY `dept_code` VARCHAR(6) NOT NULL COMMENT '部門コード';
ALTER TABLE `sales` MODIFY `start_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '部門開始日';
ALTER TABLE `sales` MODIFY `comp_code` VARCHAR(8) NOT NULL COMMENT '取引先コード';
ALTER TABLE `sales` MODIFY `emp_code` VARCHAR(10) NOT NULL COMMENT '社員コード';
ALTER TABLE `sales` MODIFY `sales_amnt` INTEGER NOT NULL COMMENT '売上金額合計';
ALTER TABLE `sales` MODIFY `cmp_tax` INTEGER NOT NULL COMMENT '消費税合計';
ALTER TABLE `sales` MODIFY `slip_comment` VARCHAR(1000) NULL COMMENT '備考';
ALTER TABLE `sales` MODIFY `updated_no` INTEGER NULL COMMENT '赤黒伝票番号';
ALTER TABLE `sales` MODIFY `orgnl_no` VARCHAR(10) NULL COMMENT '元伝票番号';
ALTER TABLE `sales` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `sales` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `sales` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `sales` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';

-- sales_detail table
ALTER TABLE `sales_detail` COMMENT = '売上明細テーブル';
ALTER TABLE `sales_detail` MODIFY `sales_no` VARCHAR(10) NOT NULL COMMENT '売上番号';
ALTER TABLE `sales_detail` MODIFY `row_no` INTEGER NOT NULL COMMENT '売上行番号';
ALTER TABLE `sales_detail` MODIFY `prod_code` VARCHAR(16) NOT NULL COMMENT '商品コード';
ALTER TABLE `sales_detail` MODIFY `prod_name` VARCHAR(10) NOT NULL COMMENT '商品名';
ALTER TABLE `sales_detail` MODIFY `unitprice` INTEGER NOT NULL COMMENT '販売単価';
ALTER TABLE `sales_detail` MODIFY `delivered_qty` INTEGER NULL COMMENT '出荷数量';
ALTER TABLE `sales_detail` MODIFY `quantity` INTEGER NOT NULL COMMENT '売上数量';
ALTER TABLE `sales_detail` MODIFY `discount` INTEGER NOT NULL COMMENT '値引金額';
ALTER TABLE `sales_detail` MODIFY `invoiced_date` DATETIME(6) NULL COMMENT '請求日';
ALTER TABLE `sales_detail` MODIFY `invoice_no` VARCHAR(10) NULL COMMENT '請求番号';
ALTER TABLE `sales_detail` MODIFY `invoice_delay_type` INTEGER NULL COMMENT '請求遅延区分';
ALTER TABLE `sales_detail` MODIFY `auto_journal_date` DATETIME(6) NULL COMMENT '自動仕訳日';
ALTER TABLE `sales_detail` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `sales_detail` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者名';
ALTER TABLE `sales_detail` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `sales_detail` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者名';
