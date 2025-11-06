-- Add comments to billing and payment tables

SET NAMES utf8mb4;

-- invoice table
ALTER TABLE `invoice` COMMENT = '請求データテーブル';
ALTER TABLE `invoice` MODIFY `invoice_no` VARCHAR(10) NOT NULL COMMENT '請求番号';
ALTER TABLE `invoice` MODIFY `invoiced_date` DATETIME(6) NULL COMMENT '請求日';
ALTER TABLE `invoice` MODIFY `comp_code` VARCHAR(8) NOT NULL COMMENT '取引先コード';
ALTER TABLE `invoice` MODIFY `cust_sub_no` INTEGER NULL COMMENT '顧客枝番';
ALTER TABLE `invoice` MODIFY `last_received` INTEGER NULL COMMENT '前回入金額';
ALTER TABLE `invoice` MODIFY `month_sales` INTEGER NULL COMMENT '当月売上額';
ALTER TABLE `invoice` MODIFY `month_received` INTEGER NULL COMMENT '当月入金額';
ALTER TABLE `invoice` MODIFY `month_invoice` INTEGER NULL COMMENT '当月請求額';
ALTER TABLE `invoice` MODIFY `cmp_tax` INTEGER NOT NULL COMMENT '消費税金額';
ALTER TABLE `invoice` MODIFY `invoice_received` INTEGER NULL COMMENT '請求消込金額';
ALTER TABLE `invoice` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `invoice` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者';
ALTER TABLE `invoice` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `invoice` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者';

-- invoice_detail table
ALTER TABLE `invoice_detail` COMMENT = '請求明細テーブル';
ALTER TABLE `invoice_detail` MODIFY `invoice_no` VARCHAR(10) NOT NULL COMMENT '請求番号';
ALTER TABLE `invoice_detail` MODIFY `row_no` INTEGER NOT NULL COMMENT '行番号';
ALTER TABLE `invoice_detail` MODIFY `sales_no` VARCHAR(10) NOT NULL COMMENT '売上番号';
ALTER TABLE `invoice_detail` MODIFY `sales_row_no` INTEGER NOT NULL COMMENT '売上行番号';
ALTER TABLE `invoice_detail` MODIFY `prod_code` VARCHAR(16) NOT NULL COMMENT '商品コード';
ALTER TABLE `invoice_detail` MODIFY `quantity` INTEGER NOT NULL COMMENT '数量';
ALTER TABLE `invoice_detail` MODIFY `unitprice` INTEGER NOT NULL COMMENT '単価';
ALTER TABLE `invoice_detail` MODIFY `amount` INTEGER NOT NULL COMMENT '金額';
ALTER TABLE `invoice_detail` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `invoice_detail` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者';
ALTER TABLE `invoice_detail` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `invoice_detail` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者';

-- credit table
ALTER TABLE `credit` COMMENT = '入金データテーブル';
ALTER TABLE `credit` MODIFY `credit_no` VARCHAR(10) NOT NULL COMMENT '入金番号';
ALTER TABLE `credit` MODIFY `credit_date` DATETIME(6) NULL COMMENT '入金日';
ALTER TABLE `credit` MODIFY `dept_code` VARCHAR(6) NOT NULL COMMENT '部門コード';
ALTER TABLE `credit` MODIFY `start_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '開始日';
ALTER TABLE `credit` MODIFY `cust_code` VARCHAR(8) NOT NULL COMMENT '顧客コード';
ALTER TABLE `credit` MODIFY `cust_sub_no` INTEGER NULL COMMENT '顧客枝番';
ALTER TABLE `credit` MODIFY `pay_method_type` INTEGER NULL COMMENT '支払方法区分';
ALTER TABLE `credit` MODIFY `bank_acut_code` VARCHAR(8) NULL COMMENT '入金口座コード';
ALTER TABLE `credit` MODIFY `received_amnt` INTEGER NULL COMMENT '入金金額';
ALTER TABLE `credit` MODIFY `received` INTEGER NULL COMMENT '消込金額';
ALTER TABLE `credit` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `credit` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者';
ALTER TABLE `credit` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `credit` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者';
ALTER TABLE `credit` MODIFY `update_plg_date` DATETIME(6) NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT 'プログラム更新日時';
ALTER TABLE `credit` MODIFY `update_pgm` VARCHAR(50) NULL COMMENT '更新プログラム名';

-- payment table
ALTER TABLE `payment` COMMENT = '支払データテーブル';
ALTER TABLE `payment` MODIFY `pay_no` VARCHAR(10) NOT NULL COMMENT '支払番号';
ALTER TABLE `payment` MODIFY `pay_date` INTEGER NULL COMMENT '支払日';
ALTER TABLE `payment` MODIFY `dept_code` VARCHAR(6) NOT NULL COMMENT '部門コード';
ALTER TABLE `payment` MODIFY `start_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '部門開始日';
ALTER TABLE `payment` MODIFY `sup_code` VARCHAR(8) NOT NULL COMMENT '仕入先コード';
ALTER TABLE `payment` MODIFY `sup_sub_no` INTEGER NULL COMMENT '仕入先枝番';
ALTER TABLE `payment` MODIFY `pay_method_type` INTEGER NULL COMMENT '支払方法区分';
ALTER TABLE `payment` MODIFY `pay_amnt` INTEGER NULL COMMENT '支払金額';
ALTER TABLE `payment` MODIFY `cmp_tax` INTEGER NOT NULL COMMENT '消費税合計';
ALTER TABLE `payment` MODIFY `complete_flg` INTEGER NOT NULL COMMENT '支払完了フラグ';
ALTER TABLE `payment` MODIFY `create_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '作成日時';
ALTER TABLE `payment` MODIFY `creator` VARCHAR(12) NULL COMMENT '作成者';
ALTER TABLE `payment` MODIFY `update_date` DATETIME(6) NOT NULL DEFAULT CURRENT_TIMESTAMP(6) COMMENT '更新日時';
ALTER TABLE `payment` MODIFY `updater` VARCHAR(12) NULL COMMENT '更新者';
